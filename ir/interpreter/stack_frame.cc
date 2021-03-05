#include "ir/interpreter/stack_frame.h"

#include "ir/interpreter/architecture.h"
#include "type/function.h"
#include "type/type.h"

namespace interpreter {

StackFrame::StackFrame(ir::NativeFn fn, base::untyped_buffer arguments,
                       base::untyped_buffer *stack)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(fn_->num_regs(), std::move(arguments)) {
  LOG("",
      "Creating a new stack frame with %u registers and argument block of size "
      "%u\n%s",
      fn_->num_regs(), regs_.data_.size(), *fn_.get());
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

}  // namespace interpreter
