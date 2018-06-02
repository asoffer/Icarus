#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include "ir/val.h"

namespace AST {
struct Expression;
}  // namespace AST

struct Context;

namespace backend {
std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

template <typename T>
T EvaluateAs(AST::Expression *expr, Context *ctx) {
  auto vals = Evaluate(expr, ctx);
  ASSERT(vals.size() == 1u);
  return std::get<T>(vals[0].value);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
