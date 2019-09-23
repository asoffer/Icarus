#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include <type_traits>

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "visitor/traditional_compilation.h"

namespace ir {
struct BlockDef;
}  // namespace ir

namespace ast {
struct Expression;
}  // namespace ast

namespace backend {
ir::Results Evaluate(type::Typed<ast::Expression const *> typed_expr,
                     visitor::TraditionalCompilation *visitor);
base::untyped_buffer EvaluateToBuffer(
    type::Typed<ast::Expression const *> typed_expr,
    visitor::TraditionalCompilation *visitor);

template <typename T>
T EvaluateAs(type::Typed<ast::Expression const *> typed_expr,
             visitor::TraditionalCompilation *visitor) {
  static_assert(std::is_trivially_copyable_v<T>);
  static_assert(!std::is_same_v<T, ir::BlockDef *>, "");
  // if (visitor->num_errors() != 0u) {
  //   visitor->DumpErrors();
  //   UNREACHABLE();
  // }

  base::untyped_buffer result_buf = EvaluateToBuffer(typed_expr, visitor);
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
