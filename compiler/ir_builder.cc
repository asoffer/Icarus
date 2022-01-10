#include "compiler/ir_builder.h"

#include <memory>

#include "absl/strings/str_cat.h"
#include "base/traverse.h"
#include "ir/blocks/group.h"
#include "type/array.h"
#include "type/cast.h"

namespace compiler {

// If the type `t` is not big, creates a new register referencing the value (or
// register) held in `value`. If `t` is big, `value` is either another register
// or the address of the big value and a new register referencing that address
// (or register) is created.
ir::Reg RegisterReferencing(IrBuilder &builder, type::Type t,
                            ir::PartialResultRef const &value) {
  if (t.is_big() or t.is<type::Pointer>()) {
    return builder.CurrentBlock()->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::addr_t>(),
        .result  = builder.CurrentGroup()->Reserve(),
    });
  } else {
    if (auto const *p = t.if_as<type::Primitive>()) {
      return p->Apply([&]<typename T>() {
        return builder.CurrentBlock()->Append(ir::RegisterInstruction<T>{
            .operand = value.get<T>(),
            .result  = builder.CurrentGroup()->Reserve(),
        });
      });
    } else if (auto const *e = t.if_as<type::Enum>()) {
      return builder.CurrentBlock()->Append(
          ir::RegisterInstruction<type::Enum::underlying_type>{
              .operand = value.get<type::Enum::underlying_type>(),
              .result  = builder.CurrentGroup()->Reserve(),
          });
    } else if (auto const *e = t.if_as<type::Flags>()) {
      return builder.CurrentBlock()->Append(
          ir::RegisterInstruction<type::Flags::underlying_type>{
              .operand = value.get<type::Flags::underlying_type>(),
              .result  = builder.CurrentGroup()->Reserve(),
          });
    } else {
      NOT_YET(t);
    }
  }
}

ir::Reg IrBuilder::Alloca(type::Type t) { return CurrentGroup()->Alloca(t); }

void IrBuilder::Call(ir::RegOr<ir::Fn> const &fn, type::Function const *f,
                     ir::PartialResultBuffer args, ir::OutParams outs) {
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
      ir::CallInstruction(f, fn, std::move(args), std::move(outs)));
}

static void ClearJumps(ir::JumpCmd const &jump, ir::BasicBlock *from) {
  jump.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
      j.block->erase_incoming(from);
    } else if constexpr (std::is_same_v<type, ir::JumpCmd::CondJump>) {
      j.true_block->erase_incoming(from);
      j.false_block->erase_incoming(from);
    }
  });
}

void IrBuilder::UncondJump(ir::BasicBlock *block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  block->insert_incoming(CurrentBlock());
  CurrentBlock()->set_jump(ir::JumpCmd::Uncond(block));
}

void IrBuilder::BlockJump(ir::Block b, ir::BasicBlock *after) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  CurrentBlock()->set_jump(ir::JumpCmd::ToBlock(b, after));
}

void IrBuilder::ReturnJump() {
  CurrentBlock()->set_jump(ir::JumpCmd::Return());
}

void IrBuilder::CondJump(ir::RegOr<bool> cond, ir::BasicBlock *true_block,
                         ir::BasicBlock *false_block) {
  ClearJumps(CurrentBlock()->jump(), CurrentBlock());
  if (cond.is_reg()) {
    true_block->insert_incoming(CurrentBlock());
    false_block->insert_incoming(CurrentBlock());
    CurrentBlock()->set_jump(
        ir::JumpCmd::Cond(cond.reg(), true_block, false_block));
  } else {
    return UncondJump(cond.value() ? true_block : false_block);
  }
}

void IrBuilder::Move(type::Typed<ir::RegOr<ir::addr_t>> to,
                     type::Typed<ir::Reg> from) {
  CurrentBlock()->Append(
      ir::MoveInstruction{.type = to.type(), .from = *from, .to = *to});
}

void IrBuilder::Copy(type::Typed<ir::RegOr<ir::addr_t>> to,
                     type::Typed<ir::Reg> from) {
  CurrentBlock()->Append(
      ir::CopyInstruction{.type = to.type(), .from = *from, .to = *to});
}

ir::Reg IrBuilder::PtrIncr(ir::RegOr<ir::addr_t> ptr, ir::RegOr<int64_t> inc,
                           type::Pointer const *t) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(ptr, inc, ir::OffsetCache::Kind::Passed)) {
    return *result;
  }
  ir::Reg result = CurrentGroup()->Reserve();
  cache.set(ptr, inc, ir::OffsetCache::Kind::Passed, result);
  if (not ptr.is_reg()) { ASSERT(ptr.value() != nullptr); }
  return CurrentBlock()->Append(ir::PtrIncrInstruction{
      .addr = ptr, .index = inc, .ptr = t, .result = result});
}

type::Typed<ir::Reg> IrBuilder::FieldRef(ir::RegOr<ir::addr_t> r,
                                         type::Struct const *t, int64_t n) {
  auto &cache = CurrentBlock()->offset_cache();
  if (auto result = cache.get(r, n, ir::OffsetCache::Kind::Into)) {
    return type::Typed<ir::Reg>(*result, t->fields()[n].type);
  }
  ir::Reg result = CurrentGroup()->Reserve();
  cache.set(r, n, ir::OffsetCache::Kind::Into, result);
  CurrentBlock()->Append(ir::StructIndexInstruction{
      .addr = r, .index = n, .struct_type = t, .result = result});
  return type::Typed<ir::Reg>(result, t->fields()[n].type);
}

ir::BasicBlock *IrBuilder::EmitDestructionPath(ast::Scope const *from,
                                               ast::Scope const *to) {
  return nullptr;
}

}  // namespace compiler
