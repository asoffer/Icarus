#include "ast/declaration.h"
#include "backend/eval.h"
#include "base/debug.h"
#include "frontend/source.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "misc/module.h"

namespace frontend {
std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod);
}  // namespace frontend

std::atomic<bool> found_errors = false;
ir::CompiledFn *main_fn = nullptr;

// Once this function exits the file is destructed and we no longer have
// access to the source lines. All verification for this module must be done
// inside this function.
Module *CompileModule(Module *mod, std::filesystem::path const *path) {
  mod->path_ = ASSERT_NOT_NULL(path);
  // TODO log an error if this fails.
  ASSIGN_OR(return nullptr, frontend::FileSrc src,
                   frontend::FileSrc::Make(*mod->path_));

  auto file_stmts = frontend::Parse(&src, mod);
  if (mod->error_log_.size() > 0) {
    mod->error_log_.Dump();
    found_errors = true;
    return mod;
  }

  {
    visitor::AssignScope visitor;
    file_stmts->assign_scope(&visitor, &mod->scope_);
  }

  Context ctx(mod);
  {
    visitor::VerifyType visitor;
    file_stmts->VerifyType(&visitor, &ctx);
  }
  mod->CompleteAllDeferredWork();

  if (ctx.num_errors() > 0) {
    // TODO Is this right?
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  {
    visitor::EmitIr visitor;
    file_stmts->EmitIr(&visitor, &ctx);
  }
  mod->CompleteAllDeferredWork();

  if (ctx.num_errors() > 0) {
    // TODO Is this right?
    ctx.DumpErrors();
    found_errors = true;
    return mod;
  }

  ctx.mod_->statements_ = std::move(*file_stmts);

  for (auto &fn : ctx.mod_->fns_) { fn->ComputeInvariants(); }
  for (auto &fn : ctx.mod_->fns_) { fn->CheckInvariants(); }

  for (auto const &stmt : ctx.mod_->statements_.content_) {
    if (auto *decl = stmt->if_as<ast::Declaration>()) {
      if (decl->id_ != "main") { continue; }
      auto f = backend::EvaluateAs<ir::AnyFunc>(decl->init_val.get(), &ctx);
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
