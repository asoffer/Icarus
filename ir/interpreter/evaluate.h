#ifndef ICARUS_IR_INTERPRETER_EVALUATE_H
#define ICARUS_IR_INTERPRETER_EVALUATE_H

#include "absl/cleanup/cleanup.h"
#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/subroutine.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/native_fn.h"

namespace interpreter {

template <typename InstSet>
void Execute(ir::NativeFn fn, ir::CompleteResultBuffer const& arguments = {}) {
  ASSERT(fn.type()->return_types().size() == 0u);
  auto save_errno = std::exchange(errno, 0);
  absl::Cleanup c = [&] { errno = save_errno; };
  // TODO actually just have a good way to construct the buffer
  LOG("Execute", "%s", fn);
  ExecutionContext ctx;
  StackFrame frame(&fn.byte_code(), ctx.stack());

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(ir::Reg::Parameter(i), argument.data(), argument.size());
  }
  ctx.Execute<InstSet>(fn, frame);
}

template <typename InstSet>
ir::CompleteResultBuffer EvaluateToBuffer(
    ir::NativeFn fn, ir::CompleteResultBuffer const& arguments) {
  LOG("EvaluateToBuffer", "%s", *fn);

  ASSERT(fn.type()->return_types().size() != 0u);

  ExecutionContext ctx;
  StackFrame frame(&fn.byte_code(), ctx.stack());

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(ir::Reg::Parameter(i), argument.data(), argument.size());
  }

  ir::CompleteResultBuffer result;
  auto outputs = fn.type()->return_types();

  core::Bytes total;
  for (auto t : outputs) { total += t.bytes(kArchitecture); }
  result.reserve_bytes(outputs.size(), total.value());
  for (size_t i = 0; i < outputs.size(); ++i) {
    frame.set<ir::addr_t>(
        ir::Reg::Output(i),
        result.append_slot(outputs[i].bytes(kArchitecture).value()));
  }

  ctx.Execute<InstSet>(fn, frame);
  return result;
}

template <typename InstSet, typename... Args>
ir::CompleteResultBuffer Evaluate(ir::NativeFn fn, Args const&... args) {
  ir::CompleteResultBuffer arguments;
  (arguments.append(args), ...);
  return EvaluateToBuffer<InstSet>(fn, arguments);
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
