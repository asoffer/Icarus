#ifndef ICARUS_IR_ANY_FUNC_H
#define ICARUS_IR_ANY_FUNC_H

#include <cstring>
#include "base/debug.h"
#include "ir/foreign.h"

namespace ir {
struct Func;

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  AnyFunc(Func *fn = nullptr) { std::memcpy(&data_, &fn, sizeof(fn)); }
  AnyFunc(ForeignFn foreign) {
    std::memcpy(&data_, &foreign.handle_, sizeof(foreign.handle_));
    data_ |= 0x1u;
  }

  Func *func() const {
    ASSERT((data_ & 0x1u) == 0u);
    Func *f;
    std::memcpy(&f, &data_, sizeof(Func *));
    return f;
  }

  ForeignFn foreign() const {
    ASSERT((data_ & 0x1u) == 1u);
    ForeignFn f;
    uintptr_t data = data_ - 1;
    std::memcpy(&f.handle_, &data, sizeof(ForeignFn));
    return f;
  }

  bool is_fn() const { return (data_ & 0x1u) == 0u; }

 private:
  inline friend bool operator==(AnyFunc lhs, AnyFunc rhs) {
    return lhs.data_ == rhs.data_;
  }
  inline friend bool operator<(AnyFunc lhs, AnyFunc rhs) {
    return lhs.data_ < rhs.data_;
  }
  inline friend bool operator>(AnyFunc lhs, AnyFunc rhs) {
    return lhs.data_ > rhs.data_;
  }

  uintptr_t data_;
};

inline std::ostream &operator<<(std::ostream &os, AnyFunc a) {
  return a.is_fn() ? (os << a.func()) : (os << a.foreign());
}

}  // namespace ir

#endif  // ICARUS_IR_ANY_FUNC_H
