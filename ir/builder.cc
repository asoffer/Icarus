#include "ir/builder.h"

#include <memory>

#include "absl/strings/str_cat.h"
#include "ir/blocks/group.h"

namespace ir {

BasicBlock *Builder::AddBlock() { return CurrentGroup()->AppendBlock(); }
BasicBlock *Builder::AddBlock(std::string header) {
  return CurrentGroup()->AppendBlock(BasicBlock::DebugInfo{
      .header = std::move(header),
  });
}

BasicBlock *Builder::AddBlock(BasicBlock const &to_copy) {
  return CurrentGroup()->AppendBlock(to_copy);
}

SetCurrent::SetCurrent(internal::BlockGroupBase &group, Builder &builder)
    : builder_(builder),
      old_group_(builder_.CurrentGroup()),
      old_block_(builder_.CurrentBlock()),
      old_termination_state_(builder_.current_.block_termination_state_) {
  builder_.CurrentGroup()  = &group;
  builder_.current_.block_ = group.entry();
  builder_.current_.block_termination_state_ =
      Builder::BlockTerminationState::kMoreStatements;
}

SetCurrent::~SetCurrent() {
  builder_.CurrentGroup()                    = old_group_;
  builder_.CurrentBlock()                    = old_block_;
  builder_.current_.block_termination_state_ = old_termination_state_;
}

Reg Builder::Alloca(type::Type t) { return CurrentGroup()->Alloca(t); }

Reg Builder::TmpAlloca(type::Type t) {
  auto reg = Alloca(t);
  current_.temporaries_to_destroy_.emplace_back(reg, t);
  return reg;
}

ir::OutParams Builder::OutParams(absl::Span<type::Type const> types) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (type::Type type : types) {
    regs.push_back(type.get()->is_big() ? TmpAlloca(type)
                                        : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

ir::OutParams Builder::OutParamsMoveInit(
    absl::Span<type::Type const> types,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (size_t i = 0; i < types.size(); ++i) {
    regs.push_back(types[i].get()->is_big() ? to[i]->reg()
                                            : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

ir::OutParams Builder::OutParamsCopyInit(
    absl::Span<type::Type const> types,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (size_t i = 0; i < types.size(); ++i) {
    regs.push_back(types[i].get()->is_big() ? to[i]->reg()
                                            : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

ir::OutParams Builder::OutParamsAssign(
    absl::Span<type::Type const> types,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (size_t i = 0; i < types.size(); ++i) {
    regs.push_back(types[i].get()->is_big() ? to[i]->reg()
                                            : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

void Builder::Call(RegOr<Fn> const &fn, type::Function const *f,
                   std::vector<Value> args, ir::OutParams outs) {
  // TODO: this call should return the constructed registers rather than forcing
  // the caller to do it.
  for (auto const &p : f->params()) {
    if (auto const ptr = p.value.type().if_as<type::Pointer>()) {
      if (auto const *prim = ptr->pointee().if_as<type::Primitive>()) {
        CurrentBlock()->load_store_cache().clear(prim->meta());
      } else {
        CurrentBlock()->load_store_cache().clear();
        break;
      }
    }
  }

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

void Builder::ChooseJump(
    std::vector<std::string_view> names, std::vector<BasicBlock *> blocks,
    std::vector<core::Arguments<type::Typed<Value>>> args) {
  CurrentBlock()->set_jump(
      JumpCmd::Choose(std::move(names), std::move(blocks), std::move(args)));
}

void Builder::Move(type::Typed<RegOr<Addr>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::MoveInstruction{.type = to.type(), .from = *from, .to = *to});
}

void Builder::Copy(type::Typed<RegOr<Addr>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::CopyInstruction{.type = to.type(), .from = *from, .to = *to});
}

type::Typed<Reg> Builder::LoadSymbol(String name, type::Type type) {
  return type::Typed<Reg>(
      CurrentBlock()->Append(LoadSymbolInstruction{
          .name = name, .type = type, .result = CurrentGroup()->Reserve()}),
      type);
}

Reg Builder::Align(RegOr<type::Type> r) {
  return CurrentBlock()->Append(
      TypeInfoInstruction{.kind   = TypeInfoInstruction::Kind::Alignment,
                          .type   = r,
                          .result = CurrentGroup()->Reserve()});
}

Reg Builder::Bytes(RegOr<type::Type> r) {
  return CurrentBlock()->Append(
      TypeInfoInstruction{.kind   = TypeInfoInstruction::Kind::Bytes,
                          .type   = r,
                          .result = CurrentGroup()->Reserve()});
}

Reg Builder::PtrIncr(RegOr<Addr> ptr, RegOr<int64_t> inc,
                     type::Pointer const *t) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(ptr, inc, OffsetCache::Kind::Passed)) {
    return *result;
  }
  Reg result = CurrentGroup()->Reserve();
  cache.set(ptr, inc, OffsetCache::Kind::Passed, result);
  return CurrentBlock()->Append(PtrIncrInstruction{
      .addr = ptr, .index = inc, .ptr = t, .result = result});
}

RegOr<uint64_t> Builder::ByteViewLength(RegOr<ir::String> val) {
  if (not val.is_reg()) { return val.value().get().size(); }
  return CurrentBlock()->Append(ByteViewLengthInstruction{
      .reg = val.reg(), .result = CurrentGroup()->Reserve()});
}

RegOr<Addr> Builder::ByteViewData(RegOr<ir::String> val) {
  if (not val.is_reg()) { return val.value().addr(); }
  return CurrentBlock()->Append(ByteViewDataInstruction{
      .reg = val.reg(), .result = CurrentGroup()->Reserve()});
}

type::Typed<Reg> Builder::FieldRef(RegOr<Addr> r, type::Tuple const *t,
                                   int64_t n) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(r, n, OffsetCache::Kind::Into)) {
    return type::Typed<Reg>(*result, t->entries()[n]);
  }
  Reg result = CurrentGroup()->Reserve();
  cache.set(r, n, OffsetCache::Kind::Into, result);
  CurrentBlock()->Append(TupleIndexInstruction{
      .addr = r, .index = n, .tuple = t, .result = result});
  return type::Typed<Reg>(result, t->entries()[n]);
}

type::Typed<Reg> Builder::FieldRef(RegOr<Addr> r, type::Struct const *t,
                                   int64_t n) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(r, n, OffsetCache::Kind::Into)) {
    return type::Typed<Reg>(*result, t->fields()[n].type);
  }
  Reg result = CurrentGroup()->Reserve();
  cache.set(r, n, OffsetCache::Kind::Into, result);
  CurrentBlock()->Append(StructIndexInstruction{
      .addr = r, .index = n, .struct_type = t, .result = result});
  return type::Typed<Reg>(result, t->fields()[n].type);
}

Reg Builder::MakeBlock(Block block, std::vector<RegOr<Fn>> befores,
                       std::vector<RegOr<Jump>> afters) {
  MakeBlockInstruction inst{.block   = block,
                            .befores = std::move(befores),
                            .afters  = std::move(afters)};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::MakeScope(Scope scope, std::vector<RegOr<Jump>> inits,
                       std::vector<RegOr<Fn>> dones,
                       absl::flat_hash_map<std::string_view, Block> blocks) {
  MakeScopeInstruction inst{.scope  = scope,
                            .inits  = std::move(inits),
                            .dones  = std::move(dones),
                            .blocks = std::move(blocks)};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Enum(
    type::Enum *e, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  EnumInstruction inst{.type              = e,
                       .names_            = std::move(names),
                       .specified_values_ = std::move(specified_values)};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::Flags(
    type::Flags *f, std::vector<std::string_view> names,
    absl::flat_hash_map<uint64_t, RegOr<uint64_t>> specified_values) {
  FlagsInstruction inst{.type              = f,
                        .names_            = std::move(names),
                        .specified_values_ = std::move(specified_values)};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

// TODO: Right now function types can be generic and have parameter names as
// part of the signature, but we don't actually have any way to emit IR for
// these.
RegOr<type::Type> Builder::Arrow(std::vector<RegOr<type::Type>> const &ins,
                                 std::vector<RegOr<type::Type>> const &outs) {
  if (absl::c_all_of(ins,
                     [](RegOr<type::Type> r) { return not r.is_reg(); }) and
      absl::c_all_of(outs,
                     [](RegOr<type::Type> r) { return not r.is_reg(); })) {
    core::Params<type::QualType> in_params;
    std::vector<type::Type> out_vec;
    in_params.reserve(ins.size());
    for (auto in : ins) {
      in_params.append(
          core::AnonymousParam(type::QualType::NonConstant(in.value())));
    }
    out_vec.reserve(outs.size());
    for (auto out : outs) { out_vec.push_back(out.value()); }
    return type::Type(type::Func(std::move(in_params), std::move(out_vec)));
  }
  ArrowInstruction inst{.lhs = std::move(ins), .rhs = std::move(outs)};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

Reg Builder::OpaqueType(module::BasicModule const *mod) {
  OpaqueTypeInstruction inst{.mod = mod, .result = CurrentGroup()->Reserve()};
  auto result = inst.result;
  CurrentBlock()->Append(std::move(inst));
  return result;
}

RegOr<type::Type> Builder::Array(RegOr<ArrayInstruction::length_t> len,
                                 RegOr<type::Type> data_type) {
  if (not len.is_reg() and not data_type.is_reg()) {
    return type::Type(type::Arr(len.value(), data_type.value()));
  }

  ArrayInstruction inst{.length = len, .data_type = data_type};
  auto result = inst.result = CurrentGroup()->Reserve();
  CurrentBlock()->Append(std::move(inst));
  return result;
}

}  // namespace ir
