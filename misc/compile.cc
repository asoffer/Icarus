#include "ast/ast.h"
#include "backend/eval.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "frontend/source/file.h"
#include "ir/compiled_fn.h"
#include "module/module.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Source *src);
}  // namespace frontend

std::atomic<bool> found_errors = false;
ir::CompiledFn *main_fn        = nullptr;

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
std::unique_ptr<module::Module> CompileModule(frontend::Source *src) {
  auto mod = std::make_unique<module::Module>();

  mod->AppendStatements(frontend::Parse(src));
  compiler::Compiler compiler(mod.get());
  mod->process([&](base::PtrSpan<ast::Node const> nodes) {
    for (ast::Node const *node : nodes) { node->VerifyType(&compiler); }
    if (compiler.num_errors() > 0) { return; }

    for (ast::Node const *node : nodes) { node->EmitValue(&compiler); }
    compiler.CompleteDeferredBodies();
    if (compiler.num_errors() > 0) { return; }

    for (ast::Node const *node : nodes) {
      if (auto const *decl = node->if_as<ast::Declaration>()) {
        if (decl->id() != "main") { continue; }
        auto f = backend::EvaluateAs<ir::AnyFunc>(
            type::Typed{decl->init_val(), compiler.type_of(decl->init_val())},
            &compiler);
        ASSERT(f.is_fn() == true);
        auto ir_fn = f.func();

        // TODO check more than one?

        // TODO need to be holding a lock when you do this.
        main_fn = ir_fn;
      } else {
        continue;
      }
    }
  });

  if (compiler.num_errors() > 0) {
    found_errors = true;
    return mod;
  }

  return mod;
}
