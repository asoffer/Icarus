#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include "base/untyped_buffer.h"
#include "ir/val.h"

namespace AST {
struct Expression;
}  // namespace AST

struct Context;

namespace backend {
base::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);
base::untyped_buffer EvaluateToBuffer(AST::Expression *expr, Context *ctx);

template <typename T>
T EvaluateAs(AST::Expression *expr, Context *ctx) {
  static_assert(std::is_trivially_copyable_v<T>);

  auto result_buf = EvaluateToBuffer(expr, ctx);
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
