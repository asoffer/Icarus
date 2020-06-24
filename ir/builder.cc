#include "ir/builder.h"

#include <memory>

#include "ir/blocks/group.h"

namespace ir {

thread_local Builder current;

Builder &GetBuilder() { return current; }

BasicBlock *Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }

BasicBlock *Builder::AddBlock(BasicBlock const &to_copy) {
  return CurrentGroup()->AppendBlock(to_copy);
}

SetCurrent::SetCurrent(internal::BlockGroupBase *group, Builder *builder)
    : builder_(builder ? builder : &GetBuilder()),
      old_group_(builder_->CurrentGroup()),
      old_block_(builder_->CurrentBlock()),
      old_termination_state_(builder_->current_.block_termination_state_) {
  builder_->CurrentGroup()  = ASSERT_NOT_NULL(group);
  builder_->current_.block_ = group->entry();
  builder_->current_.block_termination_state_ =
      Builder::BlockTerminationState::kMoreStatements;
}

SetCurrent::~SetCurrent() {
  builder_->CurrentGroup()                    = old_group_;
  builder_->CurrentBlock()                    = old_block_;
  builder_->current_.block_termination_state_ = old_termination_state_;
}

Reg Builder::Alloca(type::Type const *t) { return CurrentGroup()->Alloca(t); }

Reg Builder::TmpAlloca(type::Type const *t) {
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

void Builder::Call(RegOr<Fn> const &fn, type::Function const *f,
                   std::vector<Value> args, ir::OutParams outs) {
  // TODO this call should return the constructed registers rather than forcing
  // the caller to do it.
  CurrentBlock()->load_store_cache().clear();
  ASSERT(args.size() == f->params().size());
  CurrentBlock()->Append(
      CallInstruction(f, fn, std::move(args), std::move(outs)));
}

static void ClearJumps(JumpCmd const &jump, BasicBlock *from) {
  jump.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    using base::stringify;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->erase_incoming(from);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      j.true_block->erase_incoming(from);
      j.false_block->erase_incoming(from);
    }
  });
}

void Builder::UncondJump(BasicBlock *block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  block->insert_incoming(CurrentBlock());
  CurrentBlock()->set_jump(JumpCmd::Uncond(block));
}

void Builder::ReturnJump() {
  block_termination_state() = BlockTerminationState::kReturn;
  CurrentBlock()->set_jump(JumpCmd::Return());
}

void Builder::CondJump(RegOr<bool> cond, BasicBlock *true_block,
                       BasicBlock *false_block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  if (cond.is_reg()) {
    true_block->insert_incoming(CurrentBlock());
    false_block->insert_incoming(CurrentBlock());
    CurrentBlock()->set_jump(
        JumpCmd::Cond(cond.reg(), true_block, false_block));
  } else {
    return UncondJump(cond.value() ? true_block : false_block);
  }
}

void Builder::ChooseJump(std::vector<std::string_view> names,
                         std::vector<BasicBlock *> blocks,
                         std::vector<core::FnArgs<type::Typed<Value>>> args) {
  CurrentBlock()->set_jump(
      JumpCmd::Choose(std::move(names), std::move(blocks), std::move(args)));
}

void Builder::Init(type::Type const *t, Reg r) {
  CurrentBlock()->Append(TypeManipulationInstruction(
      TypeManipulationInstruction::Kind::Init, t, r));
}

void Builder::Destroy(type::Type const *t, Reg r) {
  CurrentBlock()->Append(TypeManipulationInstruction(
      TypeManipulationInstruction::Kind::Destroy, t, r));
}

void Builder::Move(type::Type const *t, Reg from, RegOr<Addr> to) {
  CurrentBlock()->Append(TypeManipulationInstruction(
      TypeManipulationInstruction::Kind::Move, t, from, to));
}

void Builder::Copy(type::Type const *t, Reg from, RegOr<Addr> to) {
  CurrentBlock()->Append(TypeManipulationInstruction(
      TypeManipulationInstruction::Kind::Copy, t, from, to));
}

type::Typed<Reg> Builder::LoadSymbol(String name, type::Type const *type) {
  LoadSymbolInstruction inst(name, type);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return type::Typed<Reg>(result, type);
}

Reg Builder::Align(RegOr<type::Type const *> r) {
  TypeInfoInstruction inst(TypeInfoInstruction::Kind::Alignment, r);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Bytes(RegOr<type::Type const *> r) {
  TypeInfoInstruction inst(TypeInfoInstruction::Kind::Bytes, r);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                     type::Pointer const *t) {
  PtrIncrInstruction inst(ptr, inc, t);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

RegOr<int64_t> Builder::ByteViewLength(RegOr<ir::String> val) {
  if (not val.is_reg()) { return val.value().get().size(); }
  ByteViewLengthInstruction inst(val.reg());
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

RegOr<Addr> Builder::ByteViewData(RegOr<ir::String> val) {
  if (not val.is_reg()) { return val.value().addr(); }
  ByteViewDataInstruction inst(val.reg());
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Tuple const *t,
                                int64_t n) {
  TupleIndexInstruction inst(r, n, t);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return type::Typed<Reg>(result, type::Ptr(t->entries_.at(n)));
}

type::Typed<Reg> Builder::Field(RegOr<Addr> r, type::Struct const *t,
                                int64_t n) {
  StructIndexInstruction inst(r, n, t);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return type::Typed<Reg>(result, type::Ptr(t->fields()[n].type));
}

Reg Builder::VariantType(RegOr<Addr> const &r) {
  VariantAccessInstruction inst(r, false);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::VariantValue(type::Variant const *v, RegOr<Addr> const &r) {
  VariantAccessInstruction inst(r, true);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::MakeBlock(BlockDef *block_def, std::vector<RegOr<Fn>> befores,
                       std::vector<RegOr<Jump *>> afters) {
  MakeBlockInstruction inst(block_def, std::move(befores), std::move(afters));
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::MakeScope(
    ScopeDef *scope_def, std::vector<RegOr<Jump *>> inits,
    std::vector<RegOr<Fn>> dones,
    absl::flat_hash_map<std::string_view, BlockDef *> blocks) {
  MakeScopeInstruction inst(scope_def, std::move(inits), std::move(dones),
                            std::move(blocks));
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Enum(
    module::BasicModule *mod, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  EnumerationInstruction inst(EnumerationInstruction::Kind::Enum, mod,
                              std::move(names), std::move(specified_values));
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Flags(
    module::BasicModule *mod, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  EnumerationInstruction inst(EnumerationInstruction::Kind::Flags, mod,
                              std::move(names), std::move(specified_values));
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Struct(module::BasicModule const *mod, type::Struct *s,
                    std::vector<StructField> fields,
                    std::optional<ir::Fn> dtor) {
  StructInstruction inst(mod, s, std::move(fields), dtor);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

RegOr<type::Type const *> Builder::Arrow(
    std::vector<RegOr<type::Type const *>> const &ins,
    std::vector<RegOr<type::Type const *>> const &outs) {
  if (absl::c_all_of(
          ins, [](RegOr<type::Type const *> r) { return not r.is_reg(); }) and
      absl::c_all_of(
          outs, [](RegOr<type::Type const *> r) { return not r.is_reg(); })) {
    core::Params<type::QualType> in_params;
    std::vector<type::Type const *> out_vec;
    in_params.reserve(ins.size());
    for (auto in : ins) {
      // TODO push QualType into parameters
      in_params.append(
          core::AnonymousParam(type::QualType::NonConstant(in.value())));
    }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Func(std::move(in_params), std::move(out_vec));
  }
  ArrowInstruction inst(std::move(ins), std::move(outs));
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::OpaqueType(module::BasicModule const *mod) {
  OpaqueTypeInstruction inst(mod);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

RegOr<type::Type const *> Builder::Array(RegOr<ArrayInstruction::length_t> len,
                                         RegOr<type::Type const *> data_type) {
  if (not len.is_reg() and data_type.is_reg()) {
    return type::Arr(len.value(), data_type.value());
  }

  ArrayInstruction inst(len, data_type);
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

LocalBlockInterpretation Builder::MakeLocalBlockInterpretation(
    ast::ScopeNode const *node, BasicBlock *starting_block,
    BasicBlock *landing_block) {
  absl::flat_hash_map<ast::BlockNode const *, BasicBlock *> interp_map;
  for (auto const &block : node->blocks()) {
    interp_map.emplace(&block, AddBlock());
  }

  return LocalBlockInterpretation(std::move(interp_map), starting_block,
                                  landing_block);
}

}  // namespace ir
