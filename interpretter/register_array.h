#ifndef ICARUS_INTERPRETTER_REGISTER_ARRAY_H
#define ICARUS_INTERPRETTER_REGISTER_ARRAY_H

#include "base/untyped_buffer.h"
#include "ir/reg.h"

namespace interpretter {

// Represents a a collection of registers available within a given stack frame.
struct RegisterArray {
  static constexpr size_t kMaxSize = 16;
  explicit RegisterArray(size_t num_regs, base::untyped_buffer args)
      : data_(base::untyped_buffer::MakeFull(num_regs * kMaxSize)),
        args_(std::move(args)) {
    DEBUG_LOG("RegisterArray")(args_);
  }

  void write(base::untyped_buffer const &b) { data_.write(0, b); }

  auto raw(ir::Reg r) {
    if (r.is_arg()) { return args_.raw(r.arg_value() * kMaxSize); }
    if (r.is_out()) NOT_YET();
    return data_.raw(r.value() * kMaxSize);
  }

  template <typename T>
  auto get(ir::Reg r) const {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) { return args_.get<T>(r.arg_value() * kMaxSize); }
    if (r.is_out()) NOT_YET();
    return data_.get<T>(r.value() * kMaxSize);
  }

  template <typename T>
  auto set(ir::Reg r, T const &val) {
    static_assert(sizeof(T) <= kMaxSize);
    if (r.is_arg()) { return args_.set<T>(r.arg_value() * kMaxSize, val); }
    if (r.is_out()) NOT_YET();
    return data_.set<T>(r.value() * kMaxSize, val);
  }

 private:
  base::untyped_buffer data_;
  base::untyped_buffer args_;
};

}  // namespace interpretter

#endif  // ICARUS_INTERPRETTER_REGISTER_ARRAY_H
