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
static void ReplEval(ast::Expression const *expr,
                     compiler::Compiler *compiler) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrentFunc(&fn)) {
    ir::GetBuilder().CurrentBlock() = fn.entry();

    // TODO support multiple values computed simultaneously?
    auto expr_val = compiler->EmitValue(expr);
    if (compiler->num_errors() != 0) {
      compiler->DumpErrors();
      return;
    }
    // TODO compiler->CompleteDeferredBodies();
    auto *expr_type = compiler->type_of(expr);
    if (expr_type != type::Void()) { compiler->EmitPrint(expr_type, expr_val); }
    ir::ReturnJump();
  }

  ExecContext ctx;
  Execute(&fn, base::untyped_buffer(0), {}, &ctx);
}
}  // namespace backend


struct ReplModule : public module::ExtendedModule<ReplModule> {
  ReplModule()
      : module::ExtendedModule<ReplModule>(
            [this](base::PtrSpan<ast::Node const> nodes) {
              compiler::Compiler compiler(this);
              for (ast::Node const *node : nodes) {
                if (node->is<ast::Declaration>()) {
                  auto *decl = &node->as<ast::Declaration>();

                  {
                    compiler.VerifyType(decl);
                    compiler.EmitValue(decl);
                    // TODO compiler.CompleteDeferredBodies();
                    if (compiler.num_errors() != 0) { compiler.DumpErrors(); }
                  }

                } else if (node->is<ast::Expression>()) {
                  auto *expr = &node->as<ast::Expression>();
                  backend::ReplEval(expr, &compiler);
                  fprintf(stderr, "\n");
                } else {
                  NOT_YET(*node);
                }
              }
            }) {}
};

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  frontend::ReplSource repl;
  ReplModule mod;

  // TODO Parse can fail.
  while (true) { mod.Process(frontend::Parse(&repl)); }
}
