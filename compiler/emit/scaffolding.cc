#include "compiler/emit/scaffolding.h"

#include "compiler/emit/destroy.h"

namespace compiler {

ScaffoldingCleanup EmitScaffolding(CompilationDataReference ref,
                                   ir::internal::BlockGroupBase &group,
                                   ast::Scope const &scope) {
  auto &[landing_blocks, destruction_blocks] =
      ref.state().scaffolding.emplace_back();

  for (auto const *descendant : scope.executable_descendants()) {
    landing_blocks.emplace(descendant, group.AppendBlock());

    for (auto const *s = descendant; s != scope.parent(); s = s->parent()) {
      destruction_blocks.emplace(std::pair(descendant, s), group.AppendBlock());
    }
  }

  DestructionEmitter de(ref);
  for (auto [start_end, block] : destruction_blocks) {
    auto [start, end] = start_end;
    // TODO:
    // for (auto const *id : start->ordered_nonconstant_ids()) {
    //  de.EmitDestroy(ref.context().typed(id).type(), addr(id));
    //}

    block->set_jump(ir::JumpCmd::Uncond(
        start == end ? landing_blocks.find(end)->second
                     : destruction_blocks.find(std::pair(start->parent(), end))
                           ->second));
  }

  return ScaffoldingCleanup(&ref.state());
}

}  // namespace compiler
