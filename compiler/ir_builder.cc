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
      block->set_jump(ir::JumpCmd::Uncond(
          start == end
              ? landings_.find(end)->second
              : destruction_blocks_.find(std::pair(start->parent(), end))
                    ->second));
    }
  }
  CurrentBlock() = group_->entry();
}

ir::BasicBlock *IrBuilder::EmitDestructionPath(ast::Scope const *from,
                                               ast::Scope const *to) {
  CurrentBlock()->set_jump(
      ir::JumpCmd::Uncond(destruction_blocks_.at(std::pair(from, to))));
  return landing(to);
}

ir::BasicBlock *IrBuilder::landing(ast::Scope const *s) const {
  return landings_.at(s);
}

}  // namespace compiler
