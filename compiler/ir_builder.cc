#include "compiler/ir_builder.h"

#include <memory>

#include "absl/strings/str_cat.h"
#include "base/traverse.h"
#include "ir/blocks/group.h"
#include "type/array.h"
#include "type/cast.h"

namespace compiler {

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
