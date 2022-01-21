#include "compiler/emit/scaffolding.h"

#include "compiler/emit/destroy.h"

namespace compiler {
namespace {

void InsertDestructionPathLandingAt(
    ast::Scope const *scope, DestructionEmitter &de, ir::Subroutine &subroutine,
    ir::BasicBlock *landing,
    absl::flat_hash_map<std::pair<ast::Scope const *, ast::Scope const *>,
                        ir::BasicBlock *> &destruction_blocks) {
  absl::flat_hash_map<ast::Scope const *, ir::BasicBlock *> jump_mapping = {
      {scope, landing},
  };

  scope->ForEachNonConstantDeclarationSpan(
      [&](absl::Span<ast::Scope::code_location_t const> decls) {
        auto *block        = subroutine.AppendBlock();
        de.current_block() = block;

        LOG("EmitScaffolding", "%p is a destruction block for scope (%p -> %p)",
            block, scope, decls.front().get<ast::Declaration>()->scope());
        destruction_blocks.emplace(
            std::pair(scope, decls.front().get<ast::Declaration>()->scope()),
            block);

        for (auto iter = decls.rbegin(); iter != decls.rend(); ++iter) {
          ast::Declaration const &decl = *iter->get<ast::Declaration>();
          if (decl.flags() & ast::Declaration::f_IsFnParam) { continue; }
          auto ids = decl.ids();
          for (auto iter = ids.rbegin(); iter != ids.rend(); ++iter) {
            auto const *id = &*iter;
            de.EmitDestroy(de.context().typed(id).type(), de.state().addr(id));
          }
        }

        ast::Scope const *s = decls.front().get<ast::Declaration>()->scope();
        decltype(jump_mapping)::const_iterator jump_iter;
        do {
          jump_iter = jump_mapping.find(s);
          s         = s->parent();
        } while (jump_iter == jump_mapping.end());

        block->set_jump(ir::JumpCmd::Uncond(jump_iter->second));
        jump_mapping.insert_or_assign(
            decls.front().get<ast::Declaration>()->scope(), block);
      });
}

}  // namespace

ScaffoldingCleanup EmitScaffolding(CompilationDataReference ref,
                                   ir::Subroutine &subroutine,
                                   ast::Scope const &scope) {
  auto &[stack_allocations, landing_blocks, destruction_blocks] =
      ref.state().scaffolding.emplace_back();

  scope.ForEachNonConstantDeclaration([&, allocs = &stack_allocations](
                                          ast::Declaration const *decl) {
    if (decl->flags() & ast::Declaration::f_IsFnParam) { return; }
    for (ast::Declaration::Id const &id : decl->ids()) {
      allocs->emplace(&id, subroutine.Alloca(ref.context().typed(&id).type()));
    }
  });

  DestructionEmitter de(ref);
  for (auto const *descendant : scope.executable_descendants()) {
    auto *landing = subroutine.AppendBlock();
    LOG("EmitScaffolding", "%p is the landing block for scope %p", landing,
        descendant);
    landing_blocks.emplace(descendant, landing);
    InsertDestructionPathLandingAt(descendant, de, subroutine, landing,
                                   destruction_blocks);
  }

  ref.current_block() = subroutine.entry();
  return ScaffoldingCleanup(&ref.state());
}

}  // namespace compiler
