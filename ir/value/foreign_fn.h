#ifndef ICARUS_IR_VALUE_FOREIGN_FN_H
#define ICARUS_IR_VALUE_FOREIGN_FN_H

#include <cstring>
#include <iostream>

#include "type/function.h"

namespace ir {
struct AnyFunc;

struct ForeignFn {
  explicit ForeignFn(void (*fn)(), type::Function const *t);

  using void_fn_ptr = void (*)();
  void_fn_ptr get() const;
  type::Function const *type() const;

  friend std::ostream &operator<<(std::ostream &os, ForeignFn f) {
    return os << "Foreign(id = " << f.id_ << ")";
  }

  friend bool operator==(ForeignFn lhs, ForeignFn rhs) {
    return lhs.id_ == rhs.id_;
  }

  friend bool operator!=(ForeignFn lhs, ForeignFn rhs) {
    return not (lhs == rhs);
  }

 private:
  using id_t = uintptr_t;
  ForeignFn() = default;
  ForeignFn(id_t id) : id_(id) {}

  friend struct AnyFunc;

  id_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FOREIGN_FN_H
