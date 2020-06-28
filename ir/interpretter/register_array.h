#ifndef ICARUS_IR_INTERPRETTER_REGISTER_ARRAY_H
#define ICARUS_IR_INTERPRETTER_REGISTER_ARRAY_H

#include "base/untyped_buffer.h"
#include "base/untyped_buffer_view.h"
#include "ir/value/reg.h"

namespace interpretter {

// Represents a a collection of registers available within a given stack frame.
struct RegisterArray {
  static constexpr size_t kMaxSize = 8;
  explicit RegisterArray(size_t num_regs, size_t num_args)
      : num_regs_(num_regs),
        data_(
            base::untyped_buffer::MakeFull((num_regs + num_args) * kMaxSize)) {}
  explicit RegisterArray(size_t num_regs, base::untyped_buffer args)
      : num_regs_(num_regs), data_(std::move(args)) {
    ASSERT(num_regs_ * kMaxSize <= data_.size());
  }

  auto raw(ir::Reg r) {
    if (r.is_arg()) {
      return data_.raw((r.arg_value() + num_regs_) * kMaxSize);
    }
    if (r.is_out()) NOT_YET();
    return data_.raw(r.value() * kMaxSize);
  }

  template <typename T>
  auto get(ir::Reg r) const {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) {
      return data_.get<T>((r.arg_value() + num_regs_) * kMaxSize);
    }
    if (r.is_out()) NOT_YET();
    return data_.get<T>(r.value() * kMaxSize);
  }

  template <typename T>
  auto set(ir::Reg r, T const &val) {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) {
      return data_.set<T>((r.arg_value() + num_regs_) * kMaxSize, val);
    }
    if (r.is_out()) NOT_YET();
    return data_.set<T>(r.value() * kMaxSize, val);
  }

  void set_raw(ir::Reg r, void const *src, uint16_t num_bytes) {
    ASSERT(num_bytes <= kMaxSize);
    void *dst;
    if (r.is_arg()) {
      dst = data_.raw((r.arg_value() + num_regs_) * kMaxSize);
    } else if (r.is_out()) {
      NOT_YET();
    } else {
      dst = data_.raw(r.value() * kMaxSize);
    }
    std::memcpy(dst, src, num_bytes);
  }

  // private:
  size_t num_regs_;
  base::untyped_buffer data_;
};

}  // namespace interpretter

#endif  // ICARUS_IR_INTERPRETTER_REGISTER_ARRAY_H
