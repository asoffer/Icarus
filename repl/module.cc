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

}  // namespace repl
