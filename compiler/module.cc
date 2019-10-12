#include "compiler/module.h"

#include <atomic>
#include "ast/ast.h"
#include "backend/eval.h"
#include "compiler/compiler.h"
#include "ir/compiled_fn.h"

std::atomic<ir::CompiledFn *> main_fn = nullptr;

namespace compiler {

static void CompileNodes(Compiler *compiler,
                         base::PtrSpan<ast::Node const> nodes) {
  for (ast::Node const *node : nodes) { node->VerifyType(compiler); }
  if (compiler->num_errors() > 0) { return; }

  for (ast::Node const *node : nodes) { node->EmitValue(compiler); }
  compiler->CompleteDeferredBodies();
  if (compiler->num_errors() > 0) { return; }

  for (ast::Node const *node : nodes) {
    if (auto const *decl = node->if_as<ast::Declaration>()) {
      if (decl->id() != "main") { continue; }
      auto f = backend::EvaluateAs<ir::AnyFunc>(
          type::Typed{decl->init_val(), compiler->type_of(decl->init_val())},
          compiler);
      ASSERT(f.is_fn() == true);
      auto ir_fn = f.func();

      // TODO check more than one?

      main_fn = ir_fn;
    } else {
      continue;
    }
  }
}

CompiledModule::CompiledModule()
    : module::ExtendedModule<CompiledModule>(
          [this](base::PtrSpan<ast::Node const> nodes) {
            compiler::Compiler c(this);
            CompileNodes(&c, nodes);
          }) {}
}  // namespace compiler
