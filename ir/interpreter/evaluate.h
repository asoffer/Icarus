#ifndef ICARUS_IR_INTERPRETER_EVALUATE_H
#define ICARUS_IR_INTERPRETER_EVALUATE_H

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/evaluation_result.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/native_fn.h"
#include "ir/value/value.h"
#include "type/generic_struct.h"

namespace interpreter {

template <typename InstSet>
void Execute(ir::NativeFn fn) {
  ASSERT(fn.type()->output().size() == 0u);
  // TODO actually just have a good way to construct the buffer
  LOG("Execute", "%s", fn);
  ExecutionContext ctx;
  StackFrame frame(fn, ctx.stack());
  ctx.Execute<InstSet>(fn, frame);
}

template <typename InstSet>
base::untyped_buffer EvaluateToBuffer(ir::NativeFn fn) {
  // TODO: Support multiple outputs.
  LOG("EvaluateToBuffer", "%s", fn);

  ASSERT(fn.type()->output().size() != 0u);
  core::Bytes required = fn.type()->output()[0].bytes(kArchitecture);
  auto ret_buf         = base::untyped_buffer::MakeFull(required.value());

  ExecutionContext ctx;
  StackFrame frame(fn, ctx.stack());

  frame.regs_.set<ir::addr_t>(ir::Reg::Out(0), ret_buf.raw(0));
  ctx.Execute<InstSet>(fn, frame);

  LOG("EvaluateToBuffer", "Result buffer = %s", ret_buf.to_string());
  return ret_buf;
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
