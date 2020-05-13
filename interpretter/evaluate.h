#ifndef ICARUS_INTERPRETTER_EVALUATE_H
#define ICARUS_INTERPRETTER_EVALUATE_H

#include <type_traits>

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/value/value.h"

namespace interpretter {
ir::Value Evaluate(ir::CompiledFn &&fn);
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn);

template <typename T>
T EvaluateAs(ir::CompiledFn &&fn) {
  static_assert(std::is_trivially_copyable_v<T>);
  base::untyped_buffer result_buf = EvaluateToBuffer(std::move(fn));
  ASSERT(result_buf.size() == sizeof(T));
  return result_buf.get<T>(0);
}

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_EVALUATE_H
