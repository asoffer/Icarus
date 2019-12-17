#include "interpretter/stack_frame.h"

#include "core/arch.h"
#include "type/type.h"

namespace interpretter {

StackFrame::StackFrame(ir::CompiledFn *fn,
                       base::untyped_buffer const &arguments,
                       base::untyped_buffer *stack)
    : fn_(fn),
      current_(fn_->entry()),
      prev_(fn_->entry()),
      regs_(fn_->num_regs(), fn_->num_args()) {
  // TODO this is incorrect because we no longer store these aligned.
  regs_.write(arguments);

  auto arch = core::Interpretter();
  fn->allocs().for_each([&](type::Type const *t, ir::Reg r) {
    ASSERT(t != nullptr);
    // TODO there's likely some price being paid for not storing these aligned.
    regs_.set(r, ir::Addr::Stack(stack->size()));
    stack->append_bytes(t->bytes(arch).value());
  });
}

}  // namespace interpretter
