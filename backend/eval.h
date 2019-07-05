#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include <type_traits>

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "misc/context.h"

namespace ir {
struct BlockDef;
}  // namespace ir

namespace ast {
struct Expression;
}  // namespace ast

struct Context;

namespace backend {
ir::Results Evaluate(type::Typed<ast::Expression const *> typed_expr,
                     Context *ctx);
base::untyped_buffer EvaluateToBuffer(
    type::Typed<ast::Expression const *> typed_expr, Context *ctx);

template <typename T>
T EvaluateAs(type::Typed<ast::Expression const *> typed_expr, Context *ctx) {
  static_assert(std::is_trivially_copyable_v<T>);
  static_assert(!std::is_same_v<T, ir::BlockDef *>, "");
  if (ctx->num_errors() != 0u) {
    ctx->DumpErrors();
    UNREACHABLE();
  }

  base::untyped_buffer result_buf = EvaluateToBuffer(typed_expr, ctx);
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
