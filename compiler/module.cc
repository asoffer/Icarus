#include "compiler/module.h"

#include "ast/ast.h"
#include "backend/eval.h"
#include "compiler/compiler.h"
#include "ir/compiled_fn.h"

ir::CompiledFn *main_fn = nullptr;

namespace compiler {

void CompiledModule::ProcessNewNodes(base::PtrSpan<ast::Node const> nodes) {
  for (ast::Node const *node : nodes) { node->VerifyType(compiler_); }
  if (compiler_->num_errors() > 0) { return; }

  for (ast::Node const *node : nodes) { node->EmitValue(compiler_); }
  compiler_->CompleteDeferredBodies();
  if (compiler_->num_errors() > 0) { return; }

  for (ast::Node const *node : nodes) {
    if (auto const *decl = node->if_as<ast::Declaration>()) {
      if (decl->id() != "main") { continue; }
      auto f = backend::EvaluateAs<ir::AnyFunc>(
          type::Typed{decl->init_val(), compiler_->type_of(decl->init_val())},
          compiler_);
      ASSERT(f.is_fn() == true);
      auto ir_fn = f.func();

      // TODO check more than one?

      // TODO need to be holding a lock when you do this.
      main_fn = ir_fn;
    } else {
      continue;
    }
  }
}

}  // namespace compiler
