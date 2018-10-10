#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include "base/untyped_buffer.h"
#include "context.h"
#include "ir/val.h"

namespace AST {
struct Expression;
}  // namespace AST

struct Context;

namespace backend {
base::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);
base::vector<IR::Val> Evaluate(AST::Expression *expr,
                               type::Type const *expr_type, Context *ctx);
base::untyped_buffer EvaluateToBuffer(AST::Expression *expr,
                                      type::Type const *expr_type,
                                      Context *ctx);

template <typename T>
T EvaluateAs(AST::Expression *expr, type::Type const *expr_type, Context *ctx) {
  static_assert(std::is_trivially_copyable_v<T>);
  if (ctx->num_errors() != 0u) {
    ctx->DumpErrors();
    UNREACHABLE();
  }

  base::untyped_buffer result_buf = EvaluateToBuffer(expr, expr_type, ctx);
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

template <typename T>
T EvaluateAs(AST::Expression *expr, Context *ctx) {
  return EvaluateAs<T>(expr, ctx->type_of(expr), ctx);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
