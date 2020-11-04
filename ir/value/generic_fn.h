#ifndef ICARUS_IR_VALUE_GENERIC_FN_H
#define ICARUS_IR_VALUE_GENERIC_FN_H

#include <functional>
#include <iostream>

#include "base/any_invocable.h"
#include "core/arguments.h"
#include "ir/value/native_fn.h"
#include "type/typed_value.h"

namespace ir {
struct Value;

// A `GenericFn` is a callable object which either requires some of the
// arguments passed in to be known at compile-time and/or deduces the types of
// parameters from the types of arguments.
//
// TODO: There is an important distinction to be made here: Some functions
// require arguments to be known at compile-time but the type may not depend on
// this. One possible interpretation is that this is just extra type-checking.
// Another is that we should instantiate a new version of this function. We need
// to decide if we intend these to be distinguishable in the language, and if
// not what approach we intend to take.
struct GenericFn {
  explicit GenericFn(
      base::any_invocable<NativeFn(core::Arguments<type::Typed<Value>> const &)>
          gen);

  NativeFn concrete(core::Arguments<type::Typed<Value>> const &args) const;

  friend std::ostream &operator<<(std::ostream &os, GenericFn f) {
    return os << "GenericFn(id = " << f.id_ << ")";
  }

  friend bool operator==(GenericFn lhs, GenericFn rhs) {
    return lhs.id_ == rhs.id_;
  }

  friend bool operator!=(GenericFn lhs, GenericFn rhs) {
    return not(lhs == rhs);
  }

 private:
  uintptr_t id_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_GENERIC_FN_H
