#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include "base/untyped_buffer_view.h"
#include "ir/blocks/basic.h"
#include "ir/interpreter/register_array.h"
#include "ir/value/native_fn.h"

namespace interpreter {

struct StackFrame {
  StackFrame() = delete;
  StackFrame(ir::NativeFn fn, base::untyped_buffer *stack);
  StackFrame(ir::NativeFn fn, base::untyped_buffer arguments,
             base::untyped_buffer *stack);

  void MoveTo(ir::BasicBlock const *block) {
    prev_ = std::exchange(current_, block);
  }
  void MoveTo(uintptr_t offset) {
    prev_index_ = std::exchange(current_index_, offset);
  }

  constexpr ir::BasicBlock const *current_block() const { return current_; }

  ir::NativeFn fn_;
  ir::BasicBlock const *current_;
  ir::BasicBlock const *prev_;

  uintptr_t prev_index_    = 0;
  uintptr_t current_index_ = 0;

  RegisterArray regs_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
