#include "ir/inliner.h"

#include "ir/compiled_fn.h"
#include "ir/stack_frame_allocations.h"

namespace ir {
void Inliner::Inline(Reg *r, type::Type const *t) const {
  if (r->is_arg()) {
    *r = Reg{r->arg_value() + reg_offset_};
  } else if (r->is_out()) {
    // NOT_YET();
  } else {
    *r = Reg{r->value() + reg_offset_};
  }

  if (t) {
    DEBUG_LOG("inline_reserve")("Reserving t = ", t->to_string());
    auto arch   = core::Interpretter();
    auto offset = FwdAlign(CompiledFn::Current->reg_size_, t->alignment(arch));
    CompiledFn::Current->reg_size_ = offset + t->bytes(arch);
    CompiledFn::Current->compiler_reg_to_offset_.emplace(*r, offset.value());
    ++CompiledFn::Current->num_regs_;
  }
}

void Inliner::MergeAllocations(CompiledFn *fn,
                               StackFrameAllocations const &allocs) {}
}  // namespace ir
