#ifndef ICARUS_IR_INTERPRETER_REGISTER_ARRAY_H
#define ICARUS_IR_INTERPRETER_REGISTER_ARRAY_H

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/value/reg.h"

namespace interpreter {

// Represents a a collection of registers available within a given stack frame.
struct RegisterArray {
  struct Sizes {
    size_t num_registers;
    size_t num_parameters;
    size_t num_outputs;
  };

  static constexpr size_t value_size = ir::Value::value_size_v;

  explicit RegisterArray(Sizes const &sizes)
      : sizes_(sizes),
        data_(base::untyped_buffer::MakeFull((sizes_.num_registers +
                                              sizes_.num_parameters +
                                              sizes_.num_outputs) *
                                             value_size)) {}

  std::byte const *raw(ir::Reg r) const {
    // TODO: Technically not okay, but I don't want to refactor untyped_buffer
    // right now.
    return reinterpret_cast<std::byte const *>(data_.raw(offset(r)));
  }
  std::byte *raw(ir::Reg r) {
    // TODO: Technically not okay, but I don't want to refactor untyped_buffer
    // right now.
    return reinterpret_cast<std::byte *>(data_.raw(offset(r)));
  }

  template <typename T>
  auto get(ir::Reg r) const {
    static_assert(sizeof(T) <= value_size);
    return data_.get<T>(offset(r));
  }

  template <typename T>
  auto set(ir::Reg r, T const &val) {
    static_assert(sizeof(T) <= value_size);
    return data_.set<T>(offset(r), val);
  }

  void set_raw(ir::Reg r, void const *src, uint16_t num_bytes) {
    ASSERT(num_bytes <= value_size);
    std::memcpy(data_.raw(offset(r)), src, num_bytes);
  }

 private:
  // The buffer stores all registers, then all parameters, then all outputs.
  size_t offset(ir::Reg r) const {
    size_t offset = 0;
    switch (r.kind()) {
      case ir::Reg::Kind::Output:
        offset += sizes_.num_parameters;
        [[fallthrough]];
      case ir::Reg::Kind::Argument:
        offset += sizes_.num_registers;
        [[fallthrough]];
      case ir::Reg::Kind::Value:;
    }

    switch (r.kind()) {
      case ir::Reg::Kind::Argument: offset += r.arg_value(); break;
      case ir::Reg::Kind::Output: offset += r.out_value(); break;
      case ir::Reg::Kind::Value: offset += r.value(); break;
    }
    return offset * value_size;
  }

  Sizes sizes_;
  base::untyped_buffer data_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_REGISTER_ARRAY_H
