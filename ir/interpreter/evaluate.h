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
ir::ArgumentBuffer  EvaluateToBuffer(ir::NativeFn fn) {
  // TODO: Support multiple outputs.
  LOG("EvaluateToBuffer", "%s", fn);

  ASSERT(fn.type()->output().size() != 0u);
  auto t               = fn.type()->output()[0];
  core::Bytes required = t.bytes(kArchitecture);
  auto ret_buf         = base::untyped_buffer::MakeFull(required.value());

  ExecutionContext ctx;
  StackFrame frame(fn, ctx.stack());

  // TODO: Remove cast when untyped_buffer handles std::byte
  frame.regs_.set<ir::addr_t>(ir::Reg::Out(0),
                              reinterpret_cast<ir::addr_t>(ret_buf.raw(0)));
  ctx.Execute<InstSet>(fn, frame);

  LOG("EvaluateToBuffer", "Result buffer = %s", ret_buf.to_string());

  ir::ArgumentBuffer result;
  if (t.is<type::Pointer>()) {
    result.append(ret_buf.get<ir::addr_t>(0));
  } else if (t.is<type::Function>()) {
    result.append(ret_buf.get<ir::Fn>(0));
  } else if (t.is<type::Slice>()) {
    result.append(ret_buf.get<ir::Slice>(0));
  } else if (t.is<type::Enum>()) {
    result.append(ret_buf.get<type::Enum::underlying_type>(0));
  } else if (t.is<type::Flags>()) {
    result.append(ret_buf.get<type::Flags::underlying_type>(0));
  } else if (t.is<type::Jump>()) {
    result.append(ret_buf.get<ir::Jump>(0));
  } else if (t.is<type::GenericStruct>()) {
    result.append(ret_buf.get<ir::GenericFn>(0));
  } else if (auto const *p = t.if_as<type::Primitive>()) {
    p->Apply([&]<typename T>() { result.append(ret_buf.template get<T>(0)); });
  } else {
    NOT_YET(t);
  }

  return result;
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
