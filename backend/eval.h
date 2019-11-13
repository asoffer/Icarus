#ifndef ICARUS_BACKEND_EVAL_H
#define ICARUS_BACKEND_EVAL_H

#include <type_traits>

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/results.h"

namespace ir {
struct BlockDef;
}  // namespace ir

namespace backend {
ir::Results Evaluate(ir::CompiledFn &&fn);
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn);

template <typename T>
T EvaluateAs(ir::CompiledFn &&fn) {
  static_assert(std::is_trivially_copyable_v<T>);
  static_assert(not std::is_same_v<T, ir::BlockDef *>, "");
  base::untyped_buffer result_buf = EvaluateToBuffer(std::move(fn));
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace backend

#endif  // ICARUS_BACKEND_EVAL_H
