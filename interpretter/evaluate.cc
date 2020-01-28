#include "interpretter/evaluate.h"

#include "ast/expression.h"
#include "interpretter/architecture.h"
#include "interpretter/execute.h"
#include "ir/compiled_fn.h"
#include "type/function.h"

namespace interpretter {
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  size_t bytes_needed = fn.type()->output()[0]->bytes(kArchitecture).value();
  auto ret_buf        = base::untyped_buffer::MakeFull(bytes_needed);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  ExecutionContext exec_context;
  // TODO replace 16 with kMaxSize. But actually just have a good way to
  // construct the buffer
  Execute(&fn,
          base::untyped_buffer::MakeFull(
              (fn.type()->input().size() + fn.num_regs()) * 16),
          ret_slots, &exec_context);
  return ret_buf;
}

ir::Results Evaluate(ir::CompiledFn &&fn) {
  std::vector<uint32_t> offsets;
  auto buf = EvaluateToBuffer(std::move(fn));

  offsets.reserve(fn.type()->output().size());
  auto offset = core::Bytes{0};
  for (auto *t : fn.type()->output()) {
    offset = core::FwdAlign(offset, t->alignment(kArchitecture));
    offsets.push_back(offset.value());
    offset += t->bytes(kArchitecture);
  }

  return ir::Results::FromUntypedBuffer(std::move(offsets), std::move(buf));
}

}  // namespace interpretter
