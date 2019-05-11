#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "misc/context.h"

namespace ast {
struct Expression;
}  // namespace ast

struct Context;

namespace backend {
ir::Results Evaluate(ast::Expression const *expr, Context *ctx);
ir::Results Evaluate(type::Typed<ast::Expression const *> typed_expr,
                     Context *ctx);
base::untyped_buffer EvaluateToBuffer(
    type::Typed<ast::Expression const *> typed_expr, Context *ctx);

template <typename T>
T EvaluateAs(type::Typed<ast::Expression const *> typed_expr, Context *ctx) {
  static_assert(std::is_trivially_copyable_v<T>);
  if (ctx->num_errors() != 0u) {
    ctx->DumpErrors();
    UNREACHABLE();
  }

  base::untyped_buffer result_buf = EvaluateToBuffer(typed_expr, ctx);
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

template <typename T>
T EvaluateAs(ast::Expression const *expr, Context *ctx) {
  return EvaluateAs<T>({expr, ctx->type_of(expr)}, ctx);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
