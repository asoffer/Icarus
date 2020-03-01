#ifndef ICARUS_IR_OVERLOAD_SET_H
#define ICARUS_IR_OVERLOAD_SET_H

#include <functional>
#include <optional>

#include "absl/types/span.h"
#include "core/params_ref.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"  // TODO remove once anyfunc depends on this.
#include "type/cast.h"
#include "type/function.h"
#include "type/qual_type.h"
#include "type/type.h"

namespace ir {

// An `OverloadSet` represents a collection of functions which may all be
// referenced together. Note that in a syntax tree, this is necessarily a closed
// set of expressions. However to be useful in the intermediate representation,
// we need to be able to produce new functions whenever a new generic function
// is instantiated.
struct OverloadSet {
  static std::optional<AnyFunc> Closed(
      core::Params<type::Type const *> const &) {
    return std::nullopt;
  }

  explicit OverloadSet(absl::Span<AnyFunc const> fns = {},
                       std::function<std::optional<AnyFunc>(
                           core::Params<type::Type const *> const &)>
                           create = Closed)
      : create_(std::move(create)) {
    for (AnyFunc f : fns) {
      auto *fn_type = f.is_fn() ? f.func()->type() : f.foreign().type();
      fns_.emplace_back(fn_type->params(), f);
    }
  }

  std::optional<AnyFunc> Lookup(core::FnArgs<type::QualType> const &args) {
    for (auto const &[params, fn] : fns_) {
      if (core::IsCallable(core::ParamsRef(params), args,
                           [](type::QualType a, type::Type const *p) {
                             return type::CanCast(p, a.type());
                           })) {
        return fn;
      }
    }
    return std::nullopt;
  }

 private:
  std::function<std::optional<AnyFunc>(
      core::Params<type::Type const *> const &)>
      create_;
  std::vector<
      std::pair<core::Params<type::Type const *>, std::optional<AnyFunc>>>
      fns_;
};

}  // namespace ir

#endif  // ICARUS_IR_OVERLOAD_SET_H
