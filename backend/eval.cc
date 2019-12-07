#include "backend/eval.h"

#include "ast/expression.h"
#include "backend/exec.h"
#include "core/arch.h"
#include "ir/builder.h"
#include "ir/cmd/return.h"
#include "ir/compiled_fn.h"

namespace backend {
base::untyped_buffer EvaluateToBuffer(ir::CompiledFn &&fn) {
  size_t bytes_needed =
      fn.type()->output[0]->bytes(core::Interpretter()).value();
  base::untyped_buffer ret_buf(bytes_needed);
  ret_buf.append_bytes(bytes_needed, 1);
  std::vector<ir::Addr> ret_slots;

  ret_slots.push_back(ir::Addr::Heap(ret_buf.raw(0)));
  ExecContext exec_context;
  Execute(&fn, base::untyped_buffer(0), ret_slots, &exec_context);
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
