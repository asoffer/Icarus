#include "ast/ast.h"
#include "backend/eval.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "frontend/source/file.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "visitor/assign_scope.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Source *src);
}  // namespace frontend

std::atomic<bool> found_errors = false;
ir::CompiledFn *main_fn        = nullptr;

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
std::unique_ptr<module::Module> CompileModule(frontend::Source *src) {
  auto mod = std::make_unique<module::Module>(frontend::Parse(src));
  if (mod->error_log_.size() > 0) {
    mod->error_log_.Dump();
    found_errors = true;
    return mod;
  }

  {
    visitor::AssignScope visitor;
    for (auto &stmt : mod->statements_) {
      stmt->assign_scope(&visitor, &mod->scope_);
    }
  }

  compiler::Compiler visitor(mod.get());
  for (auto const &stmt : mod->statements_) { stmt->VerifyType(&visitor); }

  if (visitor.num_errors() > 0) {
    // TODO Is this right?
    visitor.DumpErrors();
    found_errors = true;
    return mod;
  }

  for (auto const &stmt : mod->statements_) { stmt->EmitValue(&visitor); }
  visitor.CompleteDeferredBodies();

  if (visitor.num_errors() > 0) {
    // TODO Is this right?
    visitor.DumpErrors();
    found_errors = true;
    return mod;
  }

  for (auto const &stmt : mod->statements_) {
    if (auto const *decl = stmt->if_as<ast::Declaration>()) {
      if (decl->id() != "main") { continue; }
      auto f = backend::EvaluateAs<ir::AnyFunc>(
          type::Typed{decl->init_val(), visitor.type_of(decl->init_val())},
          &visitor);
      ASSERT(f.is_fn() == true);
      auto ir_fn = f.func();

      // TODO check more than one?

      // TODO need to be holding a lock when you do this.
      main_fn = ir_fn;
    } else {
      continue;
    }
  }

  return mod;
}
