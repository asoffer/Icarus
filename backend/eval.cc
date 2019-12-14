#include "backend/eval.h"

#include "ast/expression.h"
#include "core/arch.h"
#include "interpretter/execute.h"
#include "ir/builder.h"
#include "ir/cmd/return.h"
#include "ir/compiled_fn.h"

namespace backend {
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  size_t bytes_needed =
      fn.type()->output[0]->bytes(core::Interpretter()).value();
  auto ret_buf = base::untyped_buffer::MakeFull(bytes_needed);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  interpretter::ExecutionContext exec_context;
  interpretter::Execute(&fn, base::untyped_buffer(0), ret_slots, &exec_context);
  return ret_buf;
}

ir::Results Evaluate(ir::CompiledFn &&fn) {
  std::vector<uint32_t> offsets;
  auto buf = EvaluateToBuffer(std::move(fn));

  offsets.reserve(fn.type()->output.size());
  auto arch   = core::Interpretter();
  auto offset = core::Bytes{0};
  for (auto *t : fn.type()->output) {
    offset = core::FwdAlign(offset, t->alignment(arch));
    offsets.push_back(offset.value());
    offset += t->bytes(arch);
  }

  return ir::Results::FromUntypedBuffer(std::move(offsets), std::move(buf));
}

}  // namespace backend
