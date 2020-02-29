#include <cstdio>
#include <memory>
#include <vector>

#include "ast/ast.h"
#include "ast/expression.h"
#include "ast/scope/scope.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "core/params.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/repl.h"
#include "interpretter/execute.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "type/function.h"

static void ReplEval(ast::Expression const *expr,
                     compiler::Compiler *compiler) {
  // TODO is nullptr for module okay here?
  ir::CompiledFn fn(type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrent(&fn)) {
    ir::GetBuilder().CurrentBlock() = fn.entry();

    // TODO support multiple values computed simultaneously?
    auto expr_val = compiler->Visit(expr, compiler::EmitValueTag{});
    if (compiler->diag().num_consumed() != 0) { return; }
    // TODO compiler->CompleteDeferredBodies();
    auto *expr_type = compiler->type_of(expr);
    if (expr_type != type::Void()) {
      NOT_YET();
    }
    compiler->builder().ReturnJump();
  }

  interpretter::ExecutionContext ctx;
  interpretter::Execute(&fn, base::untyped_buffer(0), {}, &ctx);
}

struct ReplModule : public compiler::CompiledModule {
  ~ReplModule() override {}

  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer &diag) override {
    compiler::Compiler compiler(this, diag);
    for (ast::Node const *node : nodes) {
      if (node->is<ast::Declaration>()) {
        auto *decl = &node->as<ast::Declaration>();

        {
          compiler.Visit(decl, compiler::VerifyTypeTag{});
          compiler.Visit(decl, compiler::EmitValueTag{});
          // TODO compiler.CompleteDeferredBodies();
          if (compiler.diag().num_consumed() != 0) { return; }
        }

      } else if (node->is<ast::Expression>()) {
        auto *expr = &node->as<ast::Expression>();
        ReplEval(expr, &compiler);
        fprintf(stderr, "\n");
      } else {
        NOT_YET(*node);
      }
    }
  }
};

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  auto *repl = frontend::Source::Make<frontend::ReplSource>();
  diagnostic::StreamingConsumer diag(stderr, repl);
  ReplModule mod;

  // TODO Parse can fail.
  while (true) { mod.ProcessFromSource(repl, diag); }
}
