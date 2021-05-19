#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include "base/untyped_buffer_view.h"
#include "ir/blocks/basic.h"
#include "ir/interpreter/register_array.h"
#include "ir/value/fn.h"

namespace interpreter {

struct Stack;

struct StackFrame {
  StackFrame() = delete;
  StackFrame(ir::Fn fn, Stack& stack);
  ~StackFrame();

  ir::Fn fn() const { return fn_; }

 private:
  ir::Fn fn_;
  Stack& stack_;
  size_t frame_size_;

 public:
  // TODO: Make private.
  RegisterArray regs_;
};

struct Stack {
  Stack();

  char* Allocate(size_t bytes);
  void Deallocate(size_t bytes);

 private:
  // TODO: Tune the segment allocation strategy.
  static constexpr size_t kMinStackSegmentSizeInBytes = 4096;

  struct Segment {
    base::untyped_buffer buffer;
    size_t capacity;
  };

  std::vector<Segment> segments_;
  char* end_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
