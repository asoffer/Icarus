#ifndef ICARUS_IR_INTERPRETER_EVALUATE_H
#define ICARUS_IR_INTERPRETER_EVALUATE_H

#include "base/debug.h"
#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/interpreter/architecture.h"
#include "ir/interpreter/evaluation_result.h"
#include "ir/interpreter/execution_context.h"
#include "ir/value/value.h"
#include "type/generic_struct.h"

namespace interpreter {

template <typename InstSet>
void Execute(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() == 0u);
  // TODO actually just have a good way to construct the buffer
  LOG("Execute", "%s", fn);
  ExecutionContext ctx;
  ctx.Execute<InstSet>(&fn,
                       base::untyped_buffer::MakeFull(
                           (fn.type()->params().size() + fn.num_regs()) *
                           ir::Value::value_size_v),
                       {});
}

// TODO wrap output in expected.
template <typename InstSet>
void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots) {
  ExecutionContext ctx;
  ctx.Execute<InstSet>(fn, std::move(arguments), ret_slots);
}

template <typename InstSet>
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() != 0u);
  core::Bytes required = fn.type()->output()[0].bytes(kArchitecture);
  auto ret_buf         = base::untyped_buffer::MakeFull(required.value());
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  // TODO actually just have a good way to construct the buffer
  LOG("EvaluateToBuffer", "%s", fn);
  ExecutionContext ctx;
  ctx.Execute<InstSet>(&fn,
                       base::untyped_buffer::MakeFull(
                           (fn.type()->params().size() + fn.num_regs()) *
                           ir::Value::value_size_v),
                       ret_slots);
  LOG("EvaluateToBuffer", "Result buffer = %s", ret_buf.to_string());
  return ret_buf;
}

// TODO: why an r-value reference?
template <typename InstSet>
EvaluationResult Evaluate(ir::CompiledFn &&fn) {
  LOG("Evaluate", "%s", fn);
  auto buf = EvaluateToBuffer<InstSet>(std::move(fn));
  std::vector<ir::Value> values;
  values.reserve(fn.type()->output().size());

  auto iter = buf.begin();
  for (type::Type t : fn.type()->output()) {
    if (t.get()->is_big()) {
      ir::Addr addr = iter.template read<ir::Addr>();
      values.push_back(ir::Value(addr));
    } else if (t.is<type::GenericStruct>()) {
      values.push_back(ir::Value(t));
    } else {
      type::ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                       uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                       type::Type, ir::Addr, ir::ModuleId, ir::Scope, ir::Fn,
                       ir::Jump, ir::Block, ir::GenericFn>(
          t, [&]<typename T>() {
            T val = iter.template read<T>();
            values.push_back(ir::Value(val));
          });
    }
  }

  switch (values.size()) {
    case 0: return ir::Value();
    case 1: return values[0];
    default: NOT_YET();
  }
}

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATE_H
