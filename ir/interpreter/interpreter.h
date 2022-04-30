#ifndef ICARUS_IR_INTERPRETER_INTERPRETER_H
#define ICARUS_IR_INTERPRETER_INTERPRETER_H

#include <cstddef>
#include <string>
#include <vector>

#include "base/untyped_buffer.h"
#include "ir/basic_block.h"
#include "ir/interpreter/stack_frame.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
#include "module/shared_context.h"

namespace ir::interpreter {

struct Interpreter {
  static constexpr size_t register_size = StackFrame::register_size;

  explicit Interpreter(module::SharedContext const* context)
      : context_(*ASSERT_NOT_NULL(context)) {}

  // Interprets the given `subroutine` providing `arguments` and returns the
  // results as another `CompleteResultBuffer` if execution succeeds. Returns
  // `std::nullopt` if execution fails.
  bool operator()(Subroutine const& subroutine,
                  CompleteResultBuffer const& arguments);

  // Returns the current stack frame.
  StackFrame& frame() { return frames_.back(); }

  // Pushes a new frame onto the call stack.
  void push_frame(Fn f, CompleteResultBuffer const& arguments,
                  absl::Span<addr_t const> outputs);
  void push_frame(Subroutine const* subroutine,
                  CompleteResultBuffer const& arguments,
                  absl::Span<addr_t const> outputs);

  // Removes a frame from the call stack.
  void pop_frame();

  // Indicates that a fatal error has occurred and interpretation must stop.
  void FatalError(std::string error_message) {
    fatal_error_ = std::move(error_message);
  }

 private:
  struct InstructionPointer {
    explicit InstructionPointer(Subroutine const* s)
        : basic_block(ASSERT_NOT_NULL(s)->entry()),
          iterator(s->entry()->instructions().begin()),
          previous_basic_block(nullptr) {}

    void update(BasicBlock const* next) {
      previous_basic_block = std::exchange(basic_block, next);
      iterator             = basic_block->instructions().begin();
    }

    BasicBlock const* basic_block;
    absl::Span<Inst const>::iterator iterator;
    BasicBlock const* previous_basic_block;
  };

  // Executes the interpreter from the current instruction pointer
  // (`instruction_pointers_.back()`) until an instruction either returns false
  // or the end of the basic block is reached. If an instruction returned false,
  // a null pointer is returned. Otherwise, the jump instruction at the end of
  // the block is executed and the to-be-jumped-to block is returned.
  BasicBlock const* InterpretFromInstructionPointer();

  std::vector<StackFrame> frames_;
  std::vector<InstructionPointer> instruction_pointers_;
  module::SharedContext const& context_;
  std::string fatal_error_;
};

std::optional<CompleteResultBuffer> Interpret(
    module::SharedContext const& context, Subroutine const& subroutine,
    CompleteResultBuffer const& arguments = {});

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_INTERPRETER_H
