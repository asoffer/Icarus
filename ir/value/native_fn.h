#ifndef ICARUS_IR_VALUE_NATIVE_FN_H
#define ICARUS_IR_VALUE_NATIVE_FN_H

#include <cstring>
#include <iostream>
#include <memory>
#include <vector>

#include "ir/compiled_fn.h"
#include "type/function.h"

namespace ir {
struct NativeFnSet;

// `NativeFn` represents a function callable in the language that is defined
// internally. These are "just normal functions" in the language. The are
// distinguished from foreign functions.
struct NativeFn {
  explicit NativeFn(CompiledFn *fn);

  explicit NativeFn(NativeFnSet *set, type::Function const *fn_type,
                    core::Params<type::Typed<ast::Declaration const *>> p);

  CompiledFn *get() const;
  type::Function const *type() const;

  CompiledFn *operator->() { return get(); }

  friend std::ostream &operator<<(std::ostream &os, NativeFn f) {
    return os << "NativeFn(fn = " << f.fn_ << ")";
  }

  friend bool operator==(NativeFn lhs, NativeFn rhs) {
    return lhs.fn_ == rhs.fn_;
  }

  friend bool operator!=(NativeFn lhs, NativeFn rhs) {
    return not(lhs == rhs);
  }

 private:
  friend struct Fn;

  NativeFn() = default;

  CompiledFn *fn_;
};

struct NativeFnSet {
  std::vector<std::unique_ptr<CompiledFn>> fns;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_NATIVE_FN_H
