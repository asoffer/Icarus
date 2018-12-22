#ifndef ICARUS_IR_ANY_FUNC_H
#define ICARUS_IR_ANY_FUNC_H

#include <cstring>
#include "base/debug.h"
#include "ir/foreign.h"
#include "type/function.h"

namespace ir {
using ::base::check::Is;

struct Func;

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  AnyFunc(Func *fn = nullptr) { std::memcpy(&data_, &fn, sizeof(fn)); }
  AnyFunc(Foreign foreign) {
    ASSERT(foreign.type(), Is<type::Function>());
    void *obj = foreign.get();
    std::memcpy(&data_, &obj, sizeof(void *));
    data_ |= 0x1u;
  }

  Func *func() const {
    ASSERT((data_ & 0x1u) == 0u);
    Func *f;
    std::memcpy(&f, &data_, sizeof(Func *));
    return f;
  }

  Foreign foreign() const {
    ASSERT((data_ & 0x1u) == 1u);
    Foreign f;
    uintptr_t data = data_ - 1;
    std::memcpy(&f.obj_, &data, sizeof(void *));
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
