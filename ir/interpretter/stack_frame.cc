#include "ir/interpretter/stack_frame.h"

#include "ir/interpretter/architecture.h"
#include "type/function.h"
#include "type/type.h"

namespace interpretter {

StackFrame::StackFrame(ir::NativeFn fn, base::untyped_buffer arguments,
                       base::untyped_buffer *stack)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(fn_->num_regs(), std::move(arguments)) {
  core::Bytes next_reg_loc = core::Bytes(stack->size());
  fn->for_each_alloc([&](type::Type t, ir::Reg r) {
    ASSERT(t.valid() == true);
    next_reg_loc = core::FwdAlign(next_reg_loc, t.alignment(kArchitecture));
    regs_.set(r, ir::Addr::Stack(next_reg_loc.value()));
    next_reg_loc += t.bytes(kArchitecture);
  });
  stack->append_bytes(next_reg_loc.value() - stack->size());
}

StackFrame::StackFrame(ir::NativeFn fn, base::untyped_buffer *stack)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(fn_->num_regs(), fn->type()->params().size()) {
  core::Bytes next_reg_loc = core::Bytes(stack->size());
  fn->for_each_alloc([&](type::Type t, ir::Reg r) {
    ASSERT(t.valid() == true);
    next_reg_loc = core::FwdAlign(next_reg_loc, t.alignment(kArchitecture));
    regs_.set(r, ir::Addr::Stack(next_reg_loc.value()));
    next_reg_loc += t.bytes(kArchitecture);
  });
  stack->append_bytes(next_reg_loc.value() - stack->size());
}

}  // namespace interpretter
