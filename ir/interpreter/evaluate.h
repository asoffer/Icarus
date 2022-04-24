#ifndef ICARUS_IR_INTERPRETER_EVALUATE_H
#define ICARUS_IR_INTERPRETER_EVALUATE_H

#include "absl/cleanup/cleanup.h"
#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/execution_context.h"
#include "ir/module.h"
#include "ir/subroutine.h"
#include "module/shared_context.h"

namespace interpreter {

template <typename InstSet>
void Execute(module::SharedContext const& shared_context,
             module::Module::FunctionInformation const& info,
             ir::CompleteResultBuffer const& arguments = {}) {
  ASSERT(info.type->return_types().size() == 0u);
  auto save_errno = std::exchange(errno, 0);
  absl::Cleanup c = [&] { errno = save_errno; };
  // TODO actually just have a good way to construct the buffer
  ExecutionContext ctx(&shared_context);
  LegacyStackFrame frame(info.byte_code, ctx.stack());

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(ir::Reg::Parameter(i), argument.data(), argument.size());
  }
  ctx.CallNative<InstSet>(frame);
}

template <typename InstSet>
ir::CompleteResultBuffer EvaluateToBuffer(
    module::SharedContext const& shared_context,
    module::Module::FunctionInformation const& info,
    ir::CompleteResultBuffer const& arguments) {
  ExecutionContext ctx(&shared_context);
  LegacyStackFrame frame(info.byte_code, ctx.stack());

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(ir::Reg::Parameter(i), argument.data(), argument.size());
  }

  ir::CompleteResultBuffer result;
  auto outputs = info.type->return_types();

  core::Bytes total;
  for (auto t : outputs) { total += t.bytes(kArchitecture); }
  result.reserve_bytes(outputs.size(), total.value());
  for (size_t i = 0; i < outputs.size(); ++i) {
    frame.set<ir::addr_t>(
        ir::Reg::Output(i),
        result.append_slot(outputs[i].bytes(kArchitecture).value()));
  }

  ctx.CallNative<InstSet>(frame);
  return result;
}

template <typename InstSet>
ir::CompleteResultBuffer EvaluateToBuffer(
    module::SharedContext const& shared_context, ir::Fn fn,
    ir::CompleteResultBuffer const& arguments) {
  return EvaluateToBuffer<InstSet>(shared_context, shared_context.Function(fn),
                                   arguments);
}

template <typename InstSet, typename... Args>
ir::CompleteResultBuffer Evaluate(
    module::SharedContext const& shared_context,
    module::Module::FunctionInformation const& info, Args const&... args) {
  ir::CompleteResultBuffer arguments;
  (arguments.append(args), ...);
  return EvaluateToBuffer<InstSet>(shared_context, info, arguments);
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
