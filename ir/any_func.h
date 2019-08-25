#ifndef ICARUS_IR_ANY_FUNC_H
#define ICARUS_IR_ANY_FUNC_H

#include <cstring>
#include "base/debug.h"
#include "ir/foreign.h"

namespace ir {
struct CompiledFn;

// TODO This is a terrible name. Pick something better.
struct AnyFunc {
  static_assert(alignof(CompiledFn *) <= alignof(uintptr_t));
  static_assert(alignof(Foreign) <= alignof(uintptr_t));
  static_assert(sizeof(CompiledFn *) == sizeof(uintptr_t));
  static_assert(sizeof(Foreign) == sizeof(uintptr_t));

  AnyFunc(CompiledFn *fn = nullptr) { std::memcpy(&data_, &fn, sizeof(fn)); }
  AnyFunc(Foreign foreign) {
    void *obj = foreign.get();
    std::memcpy(&data_, &obj, sizeof(void *));
    data_ |= 0x1u;
  }

  CompiledFn *func() const {
    ASSERT((data_ & 0x1u) == 0u);
    CompiledFn *f;
    std::memcpy(&f, &data_, sizeof(CompiledFn *));
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
  uintptr_t data_;
};

inline std::ostream &operator<<(std::ostream &os, AnyFunc a) {
  return a.is_fn() ? (os << a.func()) : (os << a.foreign());
}

}  // namespace ir

#endif  // ICARUS_IR_ANY_FUNC_H
