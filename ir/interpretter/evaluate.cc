#include "ir/interpretter/evaluate.h"

#include "ast/expression.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/architecture.h"
#include "ir/interpretter/execute.h"
#include "ir/jump.h"
#include "ir/value/generic_fn.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/generic_struct.h"
#include "type/util.h"

namespace interpretter {

constexpr int kMaxSize = 8;

void Execute(ir::Fn fn, base::untyped_buffer arguments,
             absl::Span<ir::Addr const> ret_slots) {
  ExecutionContext exec_ctx;
  Execute<instruction_set_t>(fn, std::move(arguments), ret_slots, &exec_ctx);
}

// TODO: Return potential errors.
void Execute(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() == 0u);
  ExecutionContext exec_context;
  // TODO actually just have a good way to construct the buffer
  DEBUG_LOG("Execute")(fn);
  Execute<instruction_set_t>(
      &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      {}, &exec_context);
}

base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  ASSERT(fn.type()->output().size() != 0u);
  size_t bytes_needed = fn.type()->output()[0]->bytes(kArchitecture).value();
  auto ret_buf        = base::untyped_buffer::MakeFull(bytes_needed);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  ExecutionContext exec_context;
  // TODO actually just have a good way to construct the buffer
  DEBUG_LOG("EvaluateToBuffer")(fn);
  Execute<instruction_set_t>(
      &fn,
      base::untyped_buffer::MakeFull(
          (fn.type()->params().size() + fn.num_regs()) * kMaxSize),
      ret_slots, &exec_context);
  return ret_buf;
}

// TODO why an r-value reference?
base::expected<ir::Value, EvaluationFailure> Evaluate(ir::CompiledFn &&fn) {
  auto buf = EvaluateToBuffer(std::move(fn));
  std::vector<ir::Value> values;
  values.reserve(fn.type()->output().size());

  auto iter = buf.begin();
  for (auto *t : fn.type()->output()) {
    if (t->is<type::GenericStruct>()) {
      values.push_back(ir::Value(t));
    } else {
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double,
                       type::Type const *, ir::EnumVal, ir::FlagsVal, ir::Addr,
                       ir::String, module::BasicModule *, ir::ScopeDef *,
                       ir::Fn, ir::Jump *, ir::BlockDef *, ir::GenericFn>(
          t, [&](auto tag) {
            using T = typename decltype(tag)::type;
            T val   = iter.read<T>();
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

}  // namespace interpretter
