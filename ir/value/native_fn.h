#ifndef ICARUS_IR_VALUE_NATIVE_FN_H
#define ICARUS_IR_VALUE_NATIVE_FN_H

#include <cstring>
#include <iostream>

#include "ir/compiled_fn.h"
#include "type/function.h"

namespace ir {

// `NativeFn` represents a function callable in the language that is defined
// internally. These are "just normal functions" in the language. The are
// distinguished from foreign functions.
struct NativeFn {
  explicit NativeFn(CompiledFn *fn);

  CompiledFn *get() const;
  type::Function const *type() const;

  CompiledFn *operator->() { return get(); }

  friend std::ostream &operator<<(std::ostream &os, NativeFn f) {
    return os << "NativeFn(id = " << f.fn_ << ")";
  }

  friend bool operator==(NativeFn lhs, NativeFn rhs) {
    return lhs.fn_ == rhs.fn_;
  }

  friend bool operator!=(NativeFn lhs, NativeFn rhs) {
    return not(lhs == rhs);
  }

 private:
  friend struct Fn;
  friend struct NativeFnSet;

  NativeFn() = default;

  CompiledFn *fn_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_NATIVE_FN_H
