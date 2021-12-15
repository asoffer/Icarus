#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include <cstddef>
#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/blocks/basic.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"

namespace interpreter {

// Represents a a collection of registers available within a given stack frame.
struct RegisterArray {
};

struct Stack;

struct StackFrame {
  static constexpr size_t register_value_size = 16;

  StackFrame() = delete;
  StackFrame(ir::Fn fn, Stack &stack);
  ~StackFrame();

  ir::Fn fn() const { return fn_; }

  std::byte const *raw(ir::Reg r) const { return data_.raw(offset(r)); }
  std::byte *raw(ir::Reg r) { return data_.raw(offset(r)); }

  template <typename T>
  auto get(ir::Reg r) const {
    static_assert(sizeof(T) <= register_value_size);
    return data_.get<T>(offset(r));
  }

  template <typename T>
  auto set(ir::Reg r, T const &val) {
    static_assert(sizeof(T) <= register_value_size);
    return data_.set<T>(offset(r), val);
  }

  void set_raw(ir::Reg r, void const *src, uint16_t num_bytes) {
    ASSERT(num_bytes <= register_value_size);
    std::memcpy(data_.raw(offset(r)), src, num_bytes);
  }

 private:
  struct Sizes {
    size_t num_registers;
    size_t num_parameters;
    size_t num_outputs;
  };

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
    return offset * register_value_size;
  }

  ir::Fn fn_;
  Stack &stack_;
  size_t frame_size_;
  Sizes sizes_;
  base::untyped_buffer data_;
};

struct Stack {
  Stack();

  std::byte* Allocate(size_t bytes);
  void Deallocate(size_t bytes);

 private:
  struct Segment {
    base::untyped_buffer buffer;
    size_t capacity;
  };

  std::vector<Segment> segments_;
  std::byte* end_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
