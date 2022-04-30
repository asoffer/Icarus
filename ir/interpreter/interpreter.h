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

  // Interprets the subroutine currently on the top of the stack, returns
  // whether interpretation was successful.
  bool operator()();

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

  void FatalError(std::string_view error_message) {
    fatal_error_.assign(error_message);
  }
  void FatalError(std::same_as<std::string> auto&& error_message) {
    fatal_error_ = std::forward<std::string>(error_message);
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
    // (`instruction_pointers_.back()`) until an instruction either returns
    // false or the end of the basic block is reached. If an instruction
    // returned false, a null pointer is returned. Otherwise, the jump
    // instruction at the end of the block is executed and the to-be-jumped-to
    // block is returned.
    BasicBlock const* InterpretFromInstructionPointer();

    std::vector<StackFrame> frames_;
    std::vector<InstructionPointer> instruction_pointers_;
    module::SharedContext const& context_;
    std::string fatal_error_;
  };

std::optional<CompleteResultBuffer> Interpret(
    module::SharedContext const& context, Subroutine const& subroutine,
    CompleteResultBuffer const& arguments = {});

std::optional<CompleteResultBuffer> Interpret(
    module::SharedContext const& context, Fn f,
    CompleteResultBuffer const& arguments = {});

template <typename T>
concept FitsInRegister = (sizeof(T) <= Interpreter::register_size) and
                         std::is_trivially_copyable_v<T>;
template <typename T>
concept NotFitsInRegister = not FitsInRegister<T>;

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_INTERPRETER_H
