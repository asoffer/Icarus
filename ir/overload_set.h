#ifndef ICARUS_IR_OVERLOAD_SET_H
#define ICARUS_IR_OVERLOAD_SET_H

#include <functional>
#include <optional>

#include "absl/types/span.h"
#include "ir/any_func.h"
#include "ir/compiled_fn.h"  // TODO remove once anyfunc depends on this.
#include "type/function.h"
#include "type/type.h"

namespace ir {

// An `OverloadSet` represents a collection of functions which may all be
// referenced together. Note that in a syntax tree, this is necessarily a closed
// set of expressions. However to be useful in the intermediate representation,
// we need to be able to produce new functions whenever a new generic function
// is instantiated.
struct OverloadSet {
  static std::optional<AnyFunc> Closed(absl::Span<type::Type const *const>) {
    return std::nullopt;
  }

  explicit OverloadSet(
      absl::Span<AnyFunc const> fns = {},
      std::function<std::optional<AnyFunc>(absl::Span<type::Type const *const>)>
          create = Closed)
      : create_(std::move(create)) {
    for (AnyFunc f : fns) {
      auto *fn_type = f.is_fn() ? f.func()->type() : f.foreign().type();
      auto [iter, inserted] = fns_.emplace(fn_type->input, f);
      static_cast<void>(inserted);
      ASSERT(inserted == true);
    }
  }

  // TODO Change to a named method. This doesn't feel as great as I thought it would.
  std::optional<AnyFunc> operator[](
      std::vector<type::Type const *> const &inputs) {
    auto [iter, inserted] = fns_.emplace(inputs, std::nullopt);
    if (not inserted) { return iter->second; }
    iter->second = create_(inputs);
    return iter->second;
  }

 private:
  std::function<std::optional<AnyFunc>(absl::Span<type::Type const *const>)>
      create_;
  absl::flat_hash_map<std::vector<type::Type const *>, std::optional<AnyFunc>> fns_;
};

}  // namespace ir

#endif // ICARUS_IR_OVERLOAD_SET_H
