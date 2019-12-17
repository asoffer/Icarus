#ifndef ICARUS_INTERPRETTER_STACK_FRAME_H
#define ICARUS_INTERPRETTER_STACK_FRAME_H

#include "interpretter/register_array.h"
#include "ir/basic_block.h"
#include "ir/compiled_fn.h"

namespace interpretter {
struct ExecutionContext;

struct StackFrame {
  StackFrame() = delete;
  StackFrame(ir::CompiledFn *fn, base::untyped_buffer arguments,
             base::untyped_buffer *stack);

  void MoveTo(ir::BasicBlock const *block) {
    prev_ = std::exchange(current_, block);
  }

  constexpr ir::BasicBlock const *current_block() const { return current_; }

  ir::CompiledFn *fn_ = nullptr;
  ir::BasicBlock const *current_;
  ir::BasicBlock const *prev_;

  RegisterArray regs_;
};

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_STACK_FRAME_H
