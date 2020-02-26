#ifndef ICARUS_IR_ANY_FUNC_H
#define ICARUS_IR_ANY_FUNC_H

#include <cstring>
#include <limits>
#include <ostream>

#include "base/debug.h"
#include "ir/value/foreign_fn.h"

namespace type {
struct Function;
}  // namespace type

namespace ir {
struct CompiledFn;

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  static_assert(alignof(CompiledFn *) <= alignof(uintptr_t));
  static_assert(alignof(ForeignFn) <= alignof(uintptr_t));
  static_assert(sizeof(CompiledFn *) == sizeof(uintptr_t));
  static_assert(sizeof(ForeignFn) == sizeof(uintptr_t));

  AnyFunc(CompiledFn *fn = nullptr) {
    std::memcpy(&data_, &fn, sizeof(fn));
    ASSERT(is_fn() == true);
  }
  AnyFunc(ForeignFn foreign) {
    uintptr_t data;
    std::memcpy(&data, &foreign, sizeof(void (*)()));
    constexpr uintptr_t high_bit =
        uintptr_t{1} << (std::numeric_limits<uintptr_t>::digits - 1);
    ASSERT((data & high_bit) == 0u);
    data_ = (data << uintptr_t{1});
    data_ |= 1;
    ASSERT(is_fn() == false);
  }

  CompiledFn *func() const {
    ASSERT(is_fn() == true);
    CompiledFn *f;
    std::memcpy(&f, &data_, sizeof(CompiledFn *));
    return f;
  }

  ForeignFn foreign() const {
    ASSERT(is_fn() == false);
    return ForeignFn(data_ >> 1);
  }

  bool is_fn() const { return (data_ & 0x1u) == 0u; }

 private:
  uintptr_t data_;
};

inline std::ostream &operator<<(std::ostream &os, AnyFunc a) {
  return a.is_fn() ? (os << a.func()) : (os << a.foreign());
}

}  // namespace ir

#endif  // ICARUS_IR_ANY_FUNC_H
