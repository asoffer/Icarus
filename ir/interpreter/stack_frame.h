#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include <array>
#include <cstddef>
#include <string>
#include <type_traits>
#include <utility>

#include "base/untyped_buffer.h"
#include "core/bytes.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir::interpreter {

struct StackFrame {
  static constexpr size_t register_size = 16;

  struct Summary {
    core::Bytes required_stack_space;
    size_t num_parameters;
    size_t num_registers;
    size_t num_outputs;
  };

  explicit StackFrame(Summary const& summary, std::string& fatal_error);

  // Returns the value of type `T` stored in register `r`.
  template <typename T>
  T resolve(Reg r) const requires(std::is_trivially_copyable_v<T>) {
    T t;
    std::memcpy(std::addressof(t), find(r), sizeof(t));
    return t;
  }

  template <typename T>
  T resolve(RegOr<T> r) const requires(std::is_trivially_copyable_v<T>) {
    return r.is_reg() ? resolve<T>(r.reg()) : r.value();
  }

  // Stores `value` into register `r`.
  template <typename T>
  void set(Reg r, T const& value) requires(std::is_trivially_copyable_v<T>) {
    std::memcpy(find(r), std::addressof(value), sizeof(value));
  }

  void set_raw(ir::Reg r, void const *src, uint16_t num_bytes);

  // Loads `num_bytes` from the address `from` and writes them to the register `to`.
  void Load(core::Bytes num_bytes, RegOr<addr_t> from, Reg to) {
    ASSERT(num_bytes.value() <= register_size);
    std::memcpy(find(to), resolve(from), num_bytes.value());
  }

  // Stores `value` into `location`.
  template <typename T>
  void Store(RegOr<T> value,
             RegOr<addr_t> location) requires(std::is_trivially_copyable_v<T>) {
    if (value.is_reg()) {
      std::memcpy(resolve(location), find(value.reg()), sizeof(T));
    } else {
      std::memcpy(resolve(location), std::addressof(value.value()), sizeof(T));
    }
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

}  // namespace ir::interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
