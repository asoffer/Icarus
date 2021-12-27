#include "ir/builder.h"

#include <memory>

#include "absl/strings/str_cat.h"
#include "base/traverse.h"
#include "ir/blocks/group.h"
#include "type/array.h"
#include "type/cast.h"

namespace ir {

// If the type `t` is not big, creates a new register referencing the value (or
// register) held in `value`. If `t` is big, `value` is either another register
// or the address of the big value and a new register referencing that address
// (or register) is created.
Reg RegisterReferencing(Builder &builder, type::Type t,
                        PartialResultRef const &value) {
  if (t.is_big() or t.is<type::Pointer>()) {
    return builder.CurrentBlock()->Append(RegisterInstruction<addr_t>{
        .operand = value.get<addr_t>(),
        .result  = builder.CurrentGroup()->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>() {
        return builder.CurrentBlock()->Append(RegisterInstruction<T>{
            .operand = value.get<T>(),
            .result  = builder.CurrentGroup()->Reserve(),
        });
      });
    } else if (auto const *e = t.if_as<type::Enum>()) {
      return builder.CurrentBlock()->Append(
          RegisterInstruction<type::Enum::underlying_type>{
              .operand = value.get<type::Enum::underlying_type>(),
              .result  = builder.CurrentGroup()->Reserve(),
          });
    } else if (auto const *e = t.if_as<type::Flags>()) {
      return builder.CurrentBlock()->Append(
          RegisterInstruction<type::Flags::underlying_type>{
              .operand = value.get<type::Flags::underlying_type>(),
              .result  = builder.CurrentGroup()->Reserve(),
          });
    } else {
      NOT_YET(t);
    }
  }
}

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

ir::OutParams Builder::OutParams(
    absl::Span<type::Type const> types,
    absl::Span<type::Typed<ir::RegOr<addr_t>> const> to) {
  std::vector<Reg> regs;
  regs.reserve(types.size());
  for (size_t i = 0; i < types.size(); ++i) {
    regs.push_back(types[i].get()->is_big()
                       ? (to.empty() ? TmpAlloca(types[i]) : to[i]->reg())
                       : CurrentGroup()->Reserve());
  }
  return ir::OutParams(std::move(regs));
}

void Builder::Call(RegOr<Fn> const &fn, type::Function const *f,
                   PartialResultBuffer args, ir::OutParams outs) {
  ASSERT(args.num_entries() == f->params().size());

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

  CurrentBlock()->Append(
      CallInstruction(f, fn, std::move(args), std::move(outs)));
}

static void ClearJumps(JumpCmd const &jump, BasicBlock *from) {
  jump.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
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

void Builder::BlockJump(Block b) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  CurrentBlock()->set_jump(JumpCmd::ToBlock(b));
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

void Builder::Move(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::MoveInstruction{.type = to.type(), .from = *from, .to = *to});
}

void Builder::Copy(type::Typed<RegOr<addr_t>> to, type::Typed<Reg> from) {
  CurrentBlock()->Append(
      ir::CopyInstruction{.type = to.type(), .from = *from, .to = *to});
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

Reg Builder::PtrIncr(RegOr<addr_t> ptr, RegOr<int64_t> inc,
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

type::Typed<Reg> Builder::FieldRef(RegOr<addr_t> r, type::Struct const *t,
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

void Builder::ApplyImplicitCasts(type::Type from, type::QualType to,
                                 PartialResultBuffer &buffer) {
  if (not type::CanCastImplicitly(from, to.type())) {
    UNREACHABLE(from, "casting implicitly to", to);
  }
  if (from == to.type()) { return; }
  if (from.is<type::Slice>() and to.type().is<type::Slice>()) { return; }

  auto const *bufptr_from_type = from.if_as<type::BufferPointer>();
  auto const *ptr_to_type      = to.type().if_as<type::Pointer>();
  if (bufptr_from_type and ptr_to_type and
      type::CanCastImplicitly(bufptr_from_type, ptr_to_type)) {
    return;
  }

  if (from == type::Integer and type::IsIntegral(to.type())) {
    to.type().as<type::Primitive>().Apply([&]<typename T>() {
      if constexpr (std::is_integral_v<T>) {
        RegOr<T> result = Cast<Integer, T>(buffer.back().get<Integer>());
        buffer.pop_back();
        buffer.append(result);
      } else {
        UNREACHABLE(typeid(T).name());
      }
    });
  }
}

void Builder::ApplyImplicitCasts(type::Type from, type::QualType to,
                                 CompleteResultBuffer &buffer) {
  if (not type::CanCastImplicitly(from, to.type())) {
    UNREACHABLE(from, "casting implicitly to", to);
  }
  if (from == to.type()) { return; }
  if (from.is<type::Slice>() and to.type().is<type::Slice>()) { return; }

  auto const *bufptr_from_type = from.if_as<type::BufferPointer>();
  auto const *ptr_to_type      = to.type().if_as<type::Pointer>();
  if (bufptr_from_type and ptr_to_type and
      type::CanCastImplicitly(bufptr_from_type, ptr_to_type)) {
    return;
  }

  if (from == type::Integer and type::IsIntegral(to.type())) {
    to.type().as<type::Primitive>().Apply([&]<typename T>() {
      if constexpr (std::is_integral_v<T>) {
        T result = Cast<Integer, T>(buffer.back().get<Integer>()).value();
        buffer.pop_back();
        buffer.append(result);
      } else {
        UNREACHABLE(typeid(T).name());
      }
    });
  }
}

}  // namespace ir
