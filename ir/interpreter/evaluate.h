#ifndef ICARUS_IR_INTERPRETER_EVALUATE_H
#define ICARUS_IR_INTERPRETER_EVALUATE_H

#include "absl/cleanup/cleanup.h"
#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/native_fn.h"
#include "type/generic_struct.h"

namespace interpreter {

template <typename InstSet>
void Execute(ir::NativeFn fn) {
  ASSERT(fn.type()->output().size() == 0u);
  auto save_errno = std::exchange(errno, 0);
  absl::Cleanup c = [&] { errno = save_errno; };
  // TODO actually just have a good way to construct the buffer
  LOG("Execute", "%s", fn);
  ExecutionContext ctx;
  StackFrame frame(fn, ctx.stack());
  ctx.Execute<InstSet>(fn, frame);
}

template <typename InstSet>
ir::CompleteResultBuffer EvaluateToBuffer(ir::NativeFn fn) {
  LOG("EvaluateToBuffer", "%s", fn);

  ASSERT(fn.type()->output().size() != 0u);

  ExecutionContext ctx;
  StackFrame frame(fn, ctx.stack());

  ir::CompleteResultBuffer result;
  auto outputs = fn.type()->output();

  core::Bytes total;
  for (auto t : outputs) { total += t.bytes(kArchitecture); }
  result.reserve_bytes(outputs.size(), total.value());
  for (size_t i = 0; i < outputs.size(); ++i) {
    frame.regs_.set<ir::addr_t>(
        ir::Reg::Out(i),
        result.append_slot(outputs[i].bytes(kArchitecture).value()));
  }

  ctx.Execute<InstSet>(fn, frame);
  return result;
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
