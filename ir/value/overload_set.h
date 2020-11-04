#ifndef ICARUS_IR_OVERLOAD_VALUE_SET_H
#define ICARUS_IR_OVERLOAD_VALUE_SET_H

#include <functional>
#include <optional>
#include <utility>

#include "absl/types/span.h"
#include "core/call.h"
#include "core/params_ref.h"
#include "ir/value/fn.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace ir {

// OverloadSet:
//
// An `OverloadSet` represents a collection of functions which may all be
// referenced simultaneously. Typically these are creating by overloading an
// identifier as in this example:
//
// ```
// square ::= (n: int64) => n * n
// square ::= (x: float64) => x * x
// ```
//
// However, members of the overload set can be generic functions:
//
// ```
// square ::= (x: float32) => x * x
// square ::= (x: float64) => x * x
// square ::= (n: $'integral) => n * n
// ```
//
// For this reason, an overload set may grow over time as new instantiations of
// a generic function are requested.
struct OverloadSet {
  static std::optional<Fn> Closed(core::Arguments<type::QualType> const &) {
    return std::nullopt;
  }

  // Construct an overload set with a pre-existing collection of `ir::Fn`s as
  // well as a `std:function` which can generate more `ir::Fn`s as a mechanism
  // to handle generics.
  explicit OverloadSet(
      std::vector<Fn> fns = {},
      std::function<std::optional<Fn>(core::Arguments<type::QualType> const &)>
          create = Closed)
      : fns_(std::move(fns)), create_(std::move(create)) {}

  // Finds a `Fn` in this overload set which can be called with the `args`. This
  // `Fn` may be an instantiation of a generic function.
  std::optional<Fn> Lookup(core::Arguments<type::QualType> const &args) {
    // TODO search doesn't need to be linear.
    for (auto const &[cached_args, index] : cache_) {
      if (args != cached_args) { continue; }
      if (index == -1) { return std::nullopt; }
      return fns_[index];
    }

    size_t i = 0;
    for (Fn const &fn : fns_) {
      if (core::IsCallable(core::ParamsRef(fn.type()->params()), args,
                           [](type::QualType a, type::QualType p) {
                             return type::CanCast(p.type(), a.type());
                           })) {
        cache_.emplace_back(args, i);
        return fn;
      }
    }

    if (auto maybe_fn = create_(args)) {
      cache_.emplace_back(args, fns_.size());
      fns_.emplace_back(*std::move(maybe_fn));
    } else {
      cache_.emplace_back(args, -1);
    }
    return std::nullopt;
  }

 private:
  std::vector<Fn> fns_;
  std::vector<std::pair<core::Arguments<type::QualType>, size_t>> cache_;
  std::function<std::optional<Fn>(core::Arguments<type::QualType> const &)>
      create_;
};

}  // namespace ir

#endif  // ICARUS_IR_OVERLOAD_VALUE_SET_H
