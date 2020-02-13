#include "interpretter/stack_frame.h"

#include "interpretter/architecture.h"
#include "type/type.h"

namespace interpretter {

StackFrame::StackFrame(ir::CompiledFn *fn, base::untyped_buffer arguments,
                       base::untyped_buffer *stack)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(fn_->num_regs(), std::move(arguments)) {
  core::Bytes next_reg_loc = core::Bytes(stack->size());
  fn->allocs().for_each([&](type::Type const *t, ir::Reg r) {
    ASSERT(t != nullptr);
    next_reg_loc = core::FwdAlign(next_reg_loc, t->alignment(kArchitecture));
    regs_.set(r, ir::Addr::Stack(next_reg_loc.value()));
    next_reg_loc += t->bytes(kArchitecture);
  });
  stack->append_bytes(next_reg_loc.value() - stack->size());
}

}  // namespace interpretter
