#ifndef ICARUS_IR_INTERPRETER_INTERPRETER_H
#define ICARUS_IR_INTERPRETER_INTERPRETER_H

#include <vector>

#include "base/untyped_buffer.h"
#include "ir/subroutine.h"

namespace ir::interpreter {

struct StackFrame {
  explicit StackFrame(Subroutine const& subroutine, size_t register_size,
                      std::string& fatal_error);

  // Returns the value of type `T` stored in register `r`.
  template <typename T>
  T resolve(Reg r) const requires(std::is_trivially_copyable_v<T>) {
    T t;
    std::memcpy(std::addressof(t), find(r), sizeof(t));
    return t;
  }

  // Stores `value` into register `r`.
  template <typename T>
  void set(Reg r, T const& value) requires(std::is_trivially_copyable_v<T>) {
    std::memcpy(find(r), std::addressof(value), sizeof(value));
  }

  // Indicates that a fatal error has occurred and interpretation must stop.
  void FatalError(std::string error_message) const {
    fatal_error_ = std::move(error_message);
  }

 private:
  // Returns a pointer into `registers_` where the value for register `r` is
  // stored.
  std::byte const* find(Reg r) const;
  std::byte* find(Reg r);

  base::untyped_buffer frame_;
  base::untyped_buffer registers_;

  // Pointers into `registers_` which indicate the start of storage for those
  // register kinds.
  std::array<std::byte*, 4> starts_;

  std::string& fatal_error_;
};

struct Interpreter {
  static constexpr size_t register_size = 16;

  void operator()(Subroutine const& subroutine);

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

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_INTERPRETER_H
