#ifndef ICARUS_IR_STACK_H
#define ICARUS_IR_STACK_H

namespace IR {
struct StackFrame;

struct LocalStack {
  // how many bytes to allocate on creation
  static constexpr size_t starting_size = 1024; // No idea if this is a good idea

  char *allocs;
  size_t used;
  size_t capacity;

  LocalStack()
      : allocs((char *)malloc(starting_size)), used(0),
        capacity(starting_size) {}

  ~LocalStack() { free(allocs); }

  
  void AddFrame(StackFrame *fr);
  void RemoveFrame(StackFrame *fr);
};

struct StackFrame {
  std::vector<Value> reg;

  LocalStack *stack;

  // const std::vector<Value>& args;
  const std::vector<Value>& args;
  const Func *const func;

  size_t alignment, size, offset;
  size_t inst_ptr;
  Block *curr_block, *prev_block;

  StackFrame(Func *f, LocalStack *local_stack, const std::vector<Value> &args);
  ~StackFrame() { stack->RemoveFrame(this); }
};
} // namespace IR

#endif // ICARUS_IR_STACK_H
