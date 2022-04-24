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

namespace ir::interpreter {

struct Interpreter {
  static constexpr size_t register_size = StackFrame::register_size;

  // Interprets the given `subroutine` writing the return values to `out` and
  // returning `true` if it is successful. If an error occurs during execution,
  // returns false.
  bool operator()(Subroutine const& subroutine,
                  CompleteResultBuffer const& arguments,
                  CompleteResultBuffer& out);

 private:
  // Interprets each instruction in the basic block `current` and returns a
  // pointer indicating the next basic block to interpret. A null pointer
  // indicates a return statement or a fatal error indicating that
  // interpretation can no longer continue. In such a case, `fatal_error_` must
  // be set to a non-empty string to indicate the error.
  BasicBlock const* InterpretBasicBlock(BasicBlock const& current);

  std::vector<StackFrame> frames_;
  std::string fatal_error_;
};

std::optional<CompleteResultBuffer> Interpret(
    Subroutine const& subroutine, CompleteResultBuffer const& arguments);

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_INTERPRETER_H
