#include "ir/builder.h"

#include <memory>

#include "ir/block_group.h"

namespace ir {

thread_local Builder current;

Builder &GetBuilder() { return current; }

BasicBlock *Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }

BasicBlock *Builder::AddBlock(BasicBlock const &to_copy) {
  return CurrentGroup()->AppendBlock(to_copy);
}

SetCurrent::SetCurrent(internal::BlockGroup *group, Builder *builder)
    : builder_(builder ? builder : &GetBuilder()),
      old_group_(builder_->CurrentGroup()),
      old_block_(builder_->CurrentBlock()) {
  builder_->CurrentGroup()  = group;
  builder_->current_.block_ = group->entry();
}

SetCurrent::~SetCurrent() {
  builder_->CurrentGroup() = old_group_;
  builder_->CurrentBlock() = old_block_;
}

base::Tagged<Addr, Reg> Builder::Alloca(type::Type const *t) {
  return CurrentGroup()->Alloca(t);
}

base::Tagged<Addr, Reg> Builder::TmpAlloca(type::Type const *t) {
  auto reg = Alloca(t);
  current_.temporaries_to_destroy_.emplace_back(reg, t);
  return reg;
}

Reg Reserve() { return current.CurrentGroup()->Reserve(); }

ir::OutParams Builder::OutParams(absl::Span<type::Type const *const> types) {
  // TODO It'd be nice to have copy/move-elision in the C++ sense. A function
  // used to initialize a variable should do so directly rather than
  // initializing a temporary allocation and then moving it.
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (type::Type const *type : types) {
    regs.push_back(type->is_big() ? TmpAlloca(type)
                                  : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

void Builder::Call(RegOr<AnyFunc> const &fn, type::Function const *f,
                   std::vector<Results> args, ir::OutParams outs) {
  // TODO this call should return the constructed registers rather than forcing
  // the caller to do it.
  CurrentBlock()->storage_cache_.clear();
  ASSERT(args.size() == f->input.size());
  auto inst = std::make_unique<CallInstruction>(f, fn, std::move(args),
                                                std::move(outs));
  CurrentBlock()->instructions_.push_back(std::move(inst));
}

static void ClearJumps(JumpCmd *jump, BasicBlock *from) {
  jump->Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    using base::stringify;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->incoming_.erase(from);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      j.true_block->incoming_.erase(from);
      j.false_block->incoming_.erase(from);
    }
  });
}

void Builder::UncondJump(BasicBlock *block) {
  ClearJumps(&CurrentBlock()->jump_, CurrentBlock());
  block->incoming_.insert(CurrentBlock());
  CurrentBlock()->jump_ = JumpCmd::Uncond(block);
}

void Builder::ReturnJump() { CurrentBlock()->jump_ = JumpCmd::Return(); }

void Builder::CondJump(RegOr<bool> cond, BasicBlock *true_block,
                       BasicBlock *false_block) {
  ClearJumps(&CurrentBlock()->jump_, CurrentBlock());
  if (cond.is_reg()) {
    true_block->incoming_.insert(CurrentBlock());
    false_block->incoming_.insert(CurrentBlock());
    CurrentBlock()->jump_ = JumpCmd::Cond(cond.reg(), true_block, false_block);
  } else {
    return UncondJump(cond.value() ? true_block : false_block);
  }
}

void Builder::ChooseJump(absl::Span<std::string_view const> names,
                         absl::Span<BasicBlock *const> blocks) {
  CurrentBlock()->jump_ = JumpCmd::Choose(names);
}

void Builder::Init(type::Type const *t, Reg r) {
  auto inst = std::make_unique<TypeManipulationInstruction>(
      TypeManipulationInstruction::Kind::Init, t, r);
  CurrentBlock()->instructions_.push_back(std::move(inst));
}

void Builder::Destroy(type::Type const *t, Reg r) {
  auto inst = std::make_unique<TypeManipulationInstruction>(
      TypeManipulationInstruction::Kind::Destroy, t, r);
  CurrentBlock()->instructions_.push_back(std::move(inst));
}

void Builder::Move(type::Type const *t, Reg from, RegOr<Addr> to) {
  auto inst = std::make_unique<TypeManipulationInstruction>(
      TypeManipulationInstruction::Kind::Move, t, from, to);
  CurrentBlock()->instructions_.push_back(std::move(inst));
}

void Builder::Copy(type::Type const *t, Reg from, RegOr<Addr> to) {
  auto inst = std::make_unique<TypeManipulationInstruction>(
      TypeManipulationInstruction::Kind::Copy, t, from, to);
  CurrentBlock()->instructions_.push_back(std::move(inst));
}

type::Typed<Reg> Builder::LoadSymbol(std::string_view name,
                                     type::Type const *type) {
  auto inst   = std::make_unique<LoadSymbolInstruction>(name, type);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return type::Typed<Reg>(result, type);
}

base::Tagged<core::Alignment, Reg> Builder::Align(RegOr<type::Type const *> r) {
  auto inst = std::make_unique<TypeInfoInstruction>(
      TypeInfoInstruction::Kind::Alignment, r);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

base::Tagged<core::Bytes, Reg> Builder::Bytes(RegOr<type::Type const *> r) {
  auto inst = std::make_unique<TypeInfoInstruction>(
      TypeInfoInstruction::Kind::Bytes, r);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

base::Tagged<Addr, Reg> Builder::PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                                         type::Pointer const *t) {
  auto inst   = std::make_unique<PtrIncrInstruction>(ptr, inc, t);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

RegOr<int64_t> Builder::ByteViewLength(RegOr<std::string_view> val) {
  if (not val.is_reg()) { return val.value().size(); }
  auto inst   = std::make_unique<ByteViewLengthInstruction>(val.reg());
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

RegOr<Addr> Builder::ByteViewData(RegOr<std::string_view> val) {
  if (not val.is_reg()) { NOT_YET(); }
  auto inst   = std::make_unique<ByteViewDataInstruction>(val.reg());
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Tuple const *t,
                                int64_t n) {
  auto inst   = std::make_unique<TupleIndexInstruction>(r, n, t);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return type::Typed<Reg>(result, type::Ptr(t->entries_.at(n)));
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Struct const *t,
                                int64_t n) {
  auto inst   = std::make_unique<StructIndexInstruction>(r, n, t);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return type::Typed<Reg>(result, type::Ptr(t->fields()[n].type));
}

Reg Builder::VariantType(RegOr<Addr> const &r) {
  auto inst   = std::make_unique<VariantAccessInstruction>(r, false);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::VariantValue(type::Variant const *v, RegOr<Addr> const &r) {
  auto inst   = std::make_unique<VariantAccessInstruction>(r, true);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::MakeBlock(ir::BlockDef *block_def,
                       std::vector<RegOr<AnyFunc>> befores,
                       std::vector<RegOr<Jump *>> afters) {
  auto inst = std::make_unique<MakeBlockInstruction>(
      block_def, std::move(befores), std::move(afters));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::MakeScope(
    ir::ScopeDef *scope_def, std::vector<RegOr<Jump *>> inits,
    std::vector<RegOr<AnyFunc>> dones,
    absl::flat_hash_map<std::string_view, BlockDef *> blocks) {
  auto inst = std::make_unique<MakeScopeInstruction>(
      scope_def, std::move(inits), std::move(dones), std::move(blocks));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::Enum(
    module::BasicModule *mod, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  auto inst = std::make_unique<EnumerationInstruction>(
      EnumerationInstruction::Kind::Enum, mod, std::move(names),
      std::move(specified_values));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::Flags(
    module::BasicModule *mod, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  auto inst = std::make_unique<EnumerationInstruction>(
      EnumerationInstruction::Kind::Flags, mod, std::move(names),
      std::move(specified_values));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::Struct(ast::Scope const *scope, std::vector<StructField> fields) {
  auto inst   = std::make_unique<StructInstruction>(scope, std::move(fields));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

RegOr<type::Function const *> Builder::Arrow(
    std::vector<RegOr<type::Type const *>> const &ins,
    std::vector<RegOr<type::Type const *>> const &outs) {
  if (absl::c_all_of(
          ins, [](RegOr<type::Type const *> r) { return not r.is_reg(); }) and
      absl::c_all_of(
          outs, [](RegOr<type::Type const *> r) { return not r.is_reg(); })) {
    std::vector<type::Type const *> in_vec, out_vec;
    in_vec.reserve(ins.size());
    for (auto in : ins) { in_vec.push_back(in.value()); }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Func(std::move(in_vec), std::move(out_vec));
  }
  auto inst =
      std::make_unique<ArrowInstruction>(std::move(ins), std::move(outs));
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

Reg Builder::OpaqueType(module::BasicModule const *mod) {
  auto inst   = std::make_unique<OpaqueTypeInstruction>(mod);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

RegOr<type::Type const *> Builder::Array(RegOr<ArrayInstruction::length_t> len,
                                         RegOr<type::Type const *> data_type) {
  if (not len.is_reg() and data_type.is_reg()) {
    return type::Arr(len.value(), data_type.value());
  }

  auto inst   = std::make_unique<ArrayInstruction>(len, data_type);
  auto result = inst->result = CurrentGroup()->Reserve();
  CurrentBlock()->instructions_.push_back(std::move(inst));
  return result;
}

LocalBlockInterpretation Builder::MakeLocalBlockInterpretation(
    ast::ScopeNode const *node) {
  absl::flat_hash_map<ast::BlockNode const *, ir::BasicBlock *> interp_map;
  for (auto const &block : node->blocks()) {
    interp_map.emplace(&block, AddBlock());
  }

  return LocalBlockInterpretation(std::move(interp_map));
}

}  // namespace ir
