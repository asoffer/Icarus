#include "run/run.h"

#include <cstdio>
#include <memory>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "backend/exec.h"
#include "base/container/vector.h"
#include "base/untyped_buffer.h"
#include "context.h"
#include "frontend/source.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"

namespace backend {
static void ReplEval(AST::Expression *expr) {
  // TODO is nullptr for module okay here?
  auto fn = std::make_unique<IR::Func>(
      nullptr, type::Func({}, {}),
      base::vector<std::pair<std::string, AST::Expression *>>{});
  CURRENT_FUNC(fn.get()) {
    IR::BasicBlock::Current = fn->entry();
    // TODO use the right module
    Context ctx(nullptr);
    // TODO support multiple values computed simultaneously?
    auto expr_val = expr->EmitIR(&ctx)[0];
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return;
    }
    auto *expr_type = ctx.type_of(expr);
    if (expr_type != type::Void()) { expr_type->EmitRepr(expr_val, &ctx); }
    IR::ReturnJump();
  }

  ExecContext ctx;
  Execute(fn.get(), base::untyped_buffer(0), {}, &ctx);
}
}  // namespace backend

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  frontend::Repl repl;
  Module mod;
  Context ctx(&mod);

repl_start : {
  auto stmts = repl.Parse(&ctx);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    goto repl_start;
  }

  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      auto *decl = &stmt->as<AST::Declaration>();
      decl->assign_scope(ctx.mod_->global_.get());
      decl->VerifyType(&ctx);
      decl->Validate(&ctx);
      decl->EmitIR(&ctx);
      if (ctx.num_errors() != 0) {
        ctx.DumpErrors();
        goto repl_start;
      }

    } else if (stmt->is<AST::Expression>()) {
      auto *expr = &stmt->as<AST::Expression>();
      expr->assign_scope(ctx.mod_->global_.get());
      backend::ReplEval(expr);
      fprintf(stderr, "\n");
    } else {
      NOT_YET(*stmt);
    }
  }
  goto repl_start;
}
}
