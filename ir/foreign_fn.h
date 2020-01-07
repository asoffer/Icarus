#ifndef ICARUS_IR_FOREIGN_FN_H
#define ICARUS_IR_FOREIGN_FN_H

#include <cstring>
#include <iostream>

// TODO depend on function directly, which involves moving core::Intepretter.
namespace type {
struct Function;
}  // namespace type

namespace ir {
struct AnyFunc;

struct ForeignFn {
  explicit ForeignFn(void (*fn)(), type::Function const *t);

  void (*get())() const { return fn_; }
  type::Function const *type() const;

  friend std::ostream &operator<<(std::ostream &os, ForeignFn f) {
    static_assert(sizeof(uintptr_t) == sizeof(void (*)()));
    uintptr_t n;
    std::memcpy(&n, &f.fn_, sizeof(n));
    return os << "Foreign(" << n << ")";
  }

 private:
  ForeignFn() {}

  friend struct AnyFunc;

  void (*fn_)() = nullptr;
};

inline bool operator==(ForeignFn lhs, ForeignFn rhs) {
  return lhs.get() == rhs.get();
}
inline bool operator<(ForeignFn lhs, ForeignFn rhs) {
  return lhs.get() < rhs.get();
}
inline bool operator>(ForeignFn lhs, ForeignFn rhs) {
  return rhs.get() < lhs.get();
}

}  // namespace ir

#endif  // ICARUS_IR_FOREIGN_FN_H
