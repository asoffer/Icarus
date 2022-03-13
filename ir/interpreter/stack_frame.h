#ifndef ICARUS_IR_INTERPRETER_STACK_FRAME_H
#define ICARUS_IR_INTERPRETER_STACK_FRAME_H

#include <cstddef>
#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/basic_block.h"
#include "ir/byte_code/byte_code.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"

namespace interpreter {

// Represents a a collection of registers available within a given stack frame.
struct RegisterArray {};

struct Stack;

struct StackFrame {
  static constexpr size_t register_value_size = 16;

  struct Summary {
    size_t num_parameters;
    size_t num_registers;
    size_t num_outputs;
    size_t num_stack_allocations;
  };

  StackFrame() = delete;
  StackFrame(ir::ByteCode const *bc, Stack &stack);
  StackFrame(Summary const &s, Stack &stack);
  ~StackFrame();

  base::untyped_buffer::const_iterator byte_code_begin() const {
    return byte_code_->begin();
  }

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
    ASSERT(offset(r) + num_bytes <= data_.size());
    std::memcpy(data_.raw(offset(r)), src, num_bytes);
  }

 private:
  // The buffer stores all stack allocations, then all registers, then all
  // parameters, then all outputs.
  size_t offset(ir::Reg r) const {
    size_t offset = 0;
    switch (r.kind()) {
      case ir::Reg::Kind::Output:
        offset += summary_.num_parameters;
        [[fallthrough]];
      case ir::Reg::Kind::Parameter:
        offset += summary_.num_registers;
        [[fallthrough]];
      case ir::Reg::Kind::Value:
        offset += summary_.num_stack_allocations;
        [[fallthrough]];
      case ir::Reg::Kind::StackAllocation:;
    }

    switch (r.kind()) {
      case ir::Reg::Kind::Parameter:
        offset += r.as<ir::Reg::Kind::Parameter>();
        break;
      case ir::Reg::Kind::Output:
        offset += r.as<ir::Reg::Kind::Output>();
        break;
      case ir::Reg::Kind::StackAllocation:
        offset += r.as<ir::Reg::Kind::StackAllocation>();
        break;
      case ir::Reg::Kind::Value: offset += r.as<ir::Reg::Kind::Value>(); break;
    }
    return offset * register_value_size;
  }

  size_t register_count() const {
    return summary_.num_registers + summary_.num_parameters +
           summary_.num_outputs + summary_.num_stack_allocations;
  }

  Stack &stack_;
  size_t frame_size_;
  ir::ByteCode const *byte_code_;
  Summary summary_;
  base::untyped_buffer data_;
};

template <typename T>
concept FitsInRegister = (sizeof(T) <= StackFrame::register_value_size) and
                         std::is_trivially_copyable_v<T>;
template <typename T>
concept NotFitsInRegister = not FitsInRegister<T>;

struct Stack {
  Stack();

  std::byte *Allocate(size_t bytes);
  void Deallocate(size_t bytes);

 private:
  struct Segment {
    base::untyped_buffer buffer;
    size_t capacity;
  };

  std::vector<Segment> segments_;
  std::byte *end_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_STACK_FRAME_H
