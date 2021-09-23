#include "repl/module.h"

#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "ast/expression.h"
#include "ast/node.h"
#include "base/log.h"
#include "base/ptr_span.h"
#include "compiler/instructions.h"
#include "compiler/resources.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"

namespace repl {
namespace {

void ReplEval(ast::Expression const *expr, compiler::Compiler *compiler) {
  ir::CompiledFn fn(type::Func({}, {}), {});
  ICARUS_SCOPE(ir::SetCurrent(fn, compiler->builder())) {
    compiler->builder().CurrentBlock() = fn.entry();

    // TODO: support multiple values computed simultaneously?
    ir::PartialResultBuffer buffer;
    compiler->EmitToBuffer(expr, buffer);
    if (compiler->diag().num_consumed() != 0) { return; }
    auto expr_type = compiler->context().qual_types(expr)[0].type();
    if (expr_type != type::Void) { NOT_YET(); }
    compiler->builder().ReturnJump();
  }

  compiler::InterpretAtCompileTime(fn);
}

}  // namespace

void Module::ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                          diagnostic::DiagnosticConsumer &diag,
                          module::Importer &importer) {
  compiler::WorkQueue work_queue;
  compiler::Compiler c({
      .context             = context(),
      .diagnostic_consumer = diag,
      .importer            = importer,
      .work_queue          = work_queue,
  });
  ir::PartialResultBuffer buffer;
  for (ast::Node const *node : nodes) {
    LOG("repl", "%s", node->DebugString());
    if (node->is<ast::Declaration>()) {
      auto *decl = &node->as<ast::Declaration>();

      {
        c.VerifyType(decl);
        buffer.clear();
        c.EmitToBuffer(decl, buffer);
        work_queue.Complete();
        if (c.diag().num_consumed() != 0) { return; }
      }

    } else if (node->is<ast::Expression>()) {
      auto *expr = &node->as<ast::Expression>();
      ReplEval(expr, &c);
      absl::FPrintF(stderr, "\n");
    } else {
      NOT_YET(*node);
    }
  }
}

}  // namespace repl
