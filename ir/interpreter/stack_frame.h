#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include "base/untyped_buffer_view.h"
#include "ir/blocks/basic.h"
#include "ir/interpreter/register_array.h"
#include "ir/value/fn.h"

namespace interpreter {

struct StackFrame {
  StackFrame() = delete;
  StackFrame(ir::Fn fn, base::untyped_buffer *stack);
  StackFrame(ir::Fn fn, base::untyped_buffer_view arguments,
             base::untyped_buffer *stack);

  ir::Fn fn() const { return fn_; }

 private:
  ir::Fn fn_;

 public:
  // TODO: Make private.
  RegisterArray regs_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
