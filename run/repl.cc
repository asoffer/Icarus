#include <cstdio>
#include <memory>

#include <vector>
#include "ast/ast.h"
#include "ast/expression.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "frontend/source/repl.h"
#include "ir/cmd/jumps.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Source *src, ::Module *mod);
}  // namespace frontend

namespace backend {
static void ReplEval(ast::Expression *expr) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(nullptr, type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrentFunc(&fn)) {
    ir::GetBuilder().CurrentBlock() = fn.entry();
    // TODO use the right module
    visitor::TraditionalCompilation visitor(nullptr);
    Context ctx(static_cast<Module *>(nullptr));

    // TODO support multiple values computed simultaneously?
    auto expr_val = visitor.EmitValue(expr);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return;
    }
    // TODO visitor.CompleteDeferredBodies();
    auto *expr_type = ctx.type_of(expr);
    if (expr_type != type::Void()) { visitor.EmitPrint(expr_type, expr_val); }
    ir::ReturnJump();
  }

  ExecContext ctx;
  Execute(&fn, base::untyped_buffer(0), {}, &ctx);
}
}  // namespace backend

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  frontend::ReplSource repl;
  Module mod;
  Context ctx(&mod);

repl_start:;
  {
    auto stmts = frontend::Parse(&repl, &mod);
    if (mod.error_log_.size() > 0) {
      mod.error_log_.Dump();
      goto repl_start;
    }

    for (auto &stmt : stmts) {
      if (stmt->is<ast::Declaration>()) {
        auto *decl = &stmt->as<ast::Declaration>();

        {
          visitor::AssignScope visitor;
          decl->assign_scope(&visitor, &mod.scope_);
        }

        {
          visitor::TraditionalCompilation visitor(&mod);
          visitor.VerifyType(decl);
          visitor.EmitValue(decl);
          // TODO visitor.CompleteDeferredBodies();
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
