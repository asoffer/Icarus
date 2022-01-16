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

IrBuilder::IrBuilder(ir::internal::BlockGroupBase *group,
                     ast::Scope const *scope)
    : group_(ASSERT_NOT_NULL(group)) {
  if (scope) {
    for (auto const *descendant : scope->executable_descendants()) {
      landings_.emplace(descendant, group_->AppendBlock());

      for (auto const *s = descendant; s != scope->parent(); s = s->parent()) {
        destruction_blocks_.emplace(std::pair(descendant, s),
                                    group_->AppendBlock());
      }
    }
    for (auto [start_end, block] : destruction_blocks_) {
      auto [start, end] = start_end;
      // for (auto const *id : start->ordered_nonconstant_ids()) {
      //   c.EmitDestroy(type::Typed<ir::Reg>(addr(id).reg(),
      //                                      c.context().qual_types(id)[0].type()));
      // }

      CurrentBlock() = block;
      UncondJump(start == end ? landings_.find(end)->second
                              : destruction_blocks_
                                    .find(std::pair(start->parent(), end))
                                    ->second);
    }
  }
  CurrentBlock() = group_->entry();
}

ir::Reg IrBuilder::Alloca(type::Type t) { return CurrentGroup()->Alloca(t); }

void IrBuilder::UncondJump(ir::BasicBlock *block) {
  CurrentBlock()->set_jump(ir::JumpCmd::Uncond(block));
}

void IrBuilder::BlockJump(ir::Block b, ir::BasicBlock *after) {
  CurrentBlock()->set_jump(ir::JumpCmd::ToBlock(b, after));
}

void IrBuilder::ReturnJump() {
  CurrentBlock()->set_jump(ir::JumpCmd::Return());
}

void IrBuilder::CondJump(ir::RegOr<bool> cond, ir::BasicBlock *true_block,
                         ir::BasicBlock *false_block) {
  if (cond.is_reg()) {
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
  UncondJump(destruction_blocks_.at(std::pair(from, to)));
  return landing(to);
}

ir::BasicBlock *IrBuilder::landing(ast::Scope const *s) const {
  return landings_.at(s);
}

}  // namespace compiler
