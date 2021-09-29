#include "compiler/work_graph.h"

namespace compiler {
namespace {

bool IsConstantDeclaration(ast::Node const *n) {
  auto const *decl = n->if_as<ast::Declaration>();
  if (not decl) { return false; }
  return (decl->flags() & ast::Declaration::f_IsConst);
}

bool IsNotConstantDeclaration(ast::Node const *n) {
  return not IsConstantDeclaration(n);
}

void VerifyNodesSatisfying(std::predicate<ast::Node const *> auto &&predicate,
                           WorkGraph &work_graph,
                           base::PtrSpan<ast::Node const> nodes) {
  Compiler c(work_graph.resources());
  for (ast::Node const *node : nodes) {
    if (not predicate(node)) { continue; }
    c.VerifyType(node);
  }
}

}  // namespace

void CompileLibrary(PersistentResources const &resources,
                    base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  w.ExecuteCompilationSequence(
      nodes,
      [](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        VerifyNodesSatisfying(IsConstantDeclaration, w, nodes);
        VerifyNodesSatisfying(IsNotConstantDeclaration, w, nodes);
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        for (auto const *node : nodes) {
          Compiler(w.resources()).EmitVoid(node);
        }
      });
}

ir::CompiledFn CompileExecutable(PersistentResources const &resources,
                                 base::PtrSpan<ast::Node const> nodes) {
  WorkGraph w(resources);
  ir::CompiledFn f = ir::CompiledFn(type::Func({}, {}), {});

  w.ExecuteCompilationSequence(
      nodes,
      [](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        VerifyNodesSatisfying(IsConstantDeclaration, w, nodes);
        VerifyNodesSatisfying(IsNotConstantDeclaration, w, nodes);
      },
      [&](WorkGraph &w, base::PtrSpan<ast::Node const> nodes) {
        Compiler c(w.resources());
        ICARUS_SCOPE(ir::SetCurrent(f, c.builder())) {
          if (nodes.empty()) {
            EmitIrForStatements(c, nodes);
          } else {
            ast::ModuleScope const &mod_scope =
                w.resources().context->module().scope();
            MakeAllStackAllocations(c, &mod_scope);
            EmitIrForStatements(c, nodes);
            MakeAllDestructions(c, &mod_scope);
            // TODO determine under which scenarios destructors can be skipped.
          }
          c.builder().ReturnJump();
        }
      });
  return f;
}

}  // namespace compiler
