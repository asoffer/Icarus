#ifndef ICARUS_COMMON_PATTERN_H
#define ICARUS_COMMON_PATTERN_H

#include "ir/type_erased_value.h"
#include "jasmin/core/function.h"

namespace ic {

struct Pattern {
  explicit constexpr Pattern(jasmin::Function<> const *f = nullptr) : f_(f) {}

  template <typename H>
  friend H AbslHashValue(H h, Pattern p) {
    return H::combine(std::move(h), p.f_);
  }

  friend bool operator==(Pattern, Pattern) = default;

  bool operator()(TypeErasedValue const &v) const {
    nth::stack<jasmin::Value> stack;
    for (jasmin::Value value : v.value()) { stack.push(value); }
    f_->invoke(stack);
    return stack.top().as<bool>();
  }

 private:
  jasmin::Function<> const *f_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_PATTERN_H
