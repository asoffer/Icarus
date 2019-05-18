#include <cstdio>
#include <memory>

#include "ast/declaration.h"
#include "ast/expression.h"
#include "core/fn_params.h"
#include "backend/exec.h"
#include <vector>
#include "base/untyped_buffer.h"
#include "frontend/source.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"

namespace frontend {
std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod);
}  // namespace frontend

namespace backend {
static void ReplEval(ast::Expression *expr) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(nullptr, type::Func({}, {}), {});
  CURRENT_FUNC(&fn) {
    ir::BasicBlock::Current = fn.entry();
    // TODO use the right module
    Context ctx(static_cast<Module *>(nullptr));

    // TODO support multiple values computed simultaneously?
    visitor::EmitIr visitor;
    auto expr_val = expr->EmitIr(&visitor, &ctx);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return;
    }
    auto *expr_type = ctx.type_of(expr);
    if (expr_type != type::Void()) {
      expr_type->EmitPrint(&visitor, expr_val, &ctx);
    }
    ir::ReturnJump();
  }

  ExecContext ctx;
  Execute(&fn, base::untyped_buffer(0), {}, &ctx);
}
}  // namespace backend

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  frontend::ReplSrc repl;
  Module mod;
  Context ctx(&mod);

repl_start:;
  {
    auto stmts = frontend::Parse(&repl, &mod);
    if (mod.error_log_.size() > 0) {
      mod.error_log_.Dump();
      goto repl_start;
    }

    for (auto &stmt : stmts->content_) {
      if (stmt->is<ast::Declaration>()) {
        auto *decl = &stmt->as<ast::Declaration>();
        {
          visitor::AssignScope visitor;
          decl->assign_scope(&visitor, &ctx.mod_->scope_);
        }
        {
          visitor::VerifyType visitor;
          decl->VerifyType(&visitor, &ctx);
        }

        {
          visitor::EmitIr visitor;
          decl->EmitIr(&visitor, &ctx);
        }
        if (ctx.num_errors() != 0) {
          ctx.DumpErrors();
          goto repl_start;
        }

      } else if (stmt->is<ast::Expression>()) {
        auto *expr = &stmt->as<ast::Expression>();
        visitor::AssignScope visitor;
        expr->assign_scope(&visitor, &ctx.mod_->scope_);
        backend::ReplEval(expr);
        fprintf(stderr, "\n");
      } else {
        NOT_YET(*stmt);
      }
    }
    goto repl_start;
  }
}
