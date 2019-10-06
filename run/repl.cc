#include <cstdio>
#include <memory>

#include <vector>
#include "ast/ast.h"
#include "ast/expression.h"
#include "backend/exec.h"
#include "base/untyped_buffer.h"
#include "core/fn_params.h"
#include "core/scope.h"
#include "frontend/parse.h"
#include "frontend/source/repl.h"
#include "ir/cmd/jumps.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "type/function.h"

namespace backend {
static void ReplEval(ast::Expression *expr) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(nullptr, type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrentFunc(&fn)) {
    ir::GetBuilder().CurrentBlock() = fn.entry();
    // TODO use the right module
    compiler::Compiler visitor(nullptr);

    // TODO support multiple values computed simultaneously?
    auto expr_val = visitor.EmitValue(expr);
    if (visitor.num_errors() != 0) {
      visitor.DumpErrors();
      return;
    }
    // TODO visitor.CompleteDeferredBodies();
    auto *expr_type = visitor.type_of(expr);
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

repl_start:;
  {
    // TODO create a new module on each parse? That's not right.
    Module mod(frontend::Parse(&repl));
    if (mod.error_log_.size() > 0) {
      mod.error_log_.Dump();
      goto repl_start;
    }

    for (auto &stmt : mod.statements_) {
      if (stmt->is<ast::Declaration>()) {
        auto *decl = &stmt->as<ast::Declaration>();

        {
          visitor::AssignScope visitor;
          decl->assign_scope(&visitor, &mod.scope_);
        }

        {
          compiler::Compiler visitor(&mod);
          visitor.VerifyType(decl);
          visitor.EmitValue(decl);
          // TODO visitor.CompleteDeferredBodies();
          if (visitor.num_errors() != 0) {
            visitor.DumpErrors();
            goto repl_start;
          }
        }

      } else if (stmt->is<ast::Expression>()) {
        auto *expr = &stmt->as<ast::Expression>();
        visitor::AssignScope visitor;
        expr->assign_scope(&visitor, &mod.scope_);
        backend::ReplEval(expr);
        fprintf(stderr, "\n");
      } else {
        NOT_YET(*stmt);
      }
    }
    goto repl_start;
  }
}
