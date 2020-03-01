#ifndef ICARUS_IR_VALUE_FOREIGN_FN_H
#define ICARUS_IR_VALUE_FOREIGN_FN_H

#include <cstring>
#include <iostream>

#include "type/function.h"

namespace ir {
// `ForeignFn` represents a function callable in the language that is defined
// externally. New foreign functions can only be created in the intermediate
// representation by calling the `foreign` builtin.
struct ForeignFn {
 private:
  using void_fn_ptr = void (*)();

 public:
  explicit ForeignFn(void (*fn)(), type::Function const *t);

  void_fn_ptr get() const;
  type::Function const *type() const;

  friend std::ostream &operator<<(std::ostream &os, ForeignFn f) {
    return os << "ForeignFn(id = " << f.id_ << ")";
  }

  friend bool operator==(ForeignFn lhs, ForeignFn rhs) {
    return lhs.id_ == rhs.id_;
  }

  friend bool operator!=(ForeignFn lhs, ForeignFn rhs) {
    return not(lhs == rhs);
  }

 private:
  using id_t  = uintptr_t;
  ForeignFn() = default;
  ForeignFn(id_t id) : id_(id) {}

  friend struct AnyFunc;
  friend struct Fn;

  id_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FOREIGN_FN_H
