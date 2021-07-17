#ifndef ICARUS_COMPILER_BOUND_PARAMETERS_H
#define ICARUS_COMPILER_BOUND_PARAMETERS_H

#include <utility>

#include "base/debug.h"
#include "core/params.h"
#include "ir/value/result_buffer.h"
#include "type/qual_type.h"

namespace compiler {
// Represents a set of parameters to a generic function, some of which are
// compile-time constants and bound to a particular value. For example,
//
// ```
// f ::= (N :: integer, a: [N; i64]) -> () { ... }
// x := [1, 4, 9]
// f(3, x)
// ```
//
// In the call to `f` above, the bound parameters would represent the value `3`
// bound to `N`, and a non-constant value of type `[3; i64]` for the parameter
// `a`. However, because `x` is not a constant, it's value is not represented,
// only its type.
struct BoundParameters {
  explicit BoundParameters(std::vector<core::Param<type::QualType>> qts,
                           absl::Span<ir::CompleteResultBuffer const> buffers);


  void append(ir::CompleteResultRef const &ref,
              core::Param<type::QualType> const &param);

  core::Params<type::QualType> const &types() const { return types_; }

  size_t size() const { return types_.size(); }
  std::pair<type::QualType, ir::CompleteResultRef> operator[](size_t i) const {
    return std::pair(types_[i].value, buffer_[i]);
  }

  friend bool operator==(BoundParameters const &lhs,
                         BoundParameters const &rhs);

  friend bool operator!=(BoundParameters const &lhs,
                         BoundParameters const &rhs) {
    return not(lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, BoundParameters const &b) {
    h        = H::combine(std::move(h), b.types_);
    size_t i = 0;
    for (auto const &value : b.types_) {
      ir::CompleteResultRef ref = b.buffer_[i++];
      if (not value.value.constant()) { continue; }
      h = H::combine(std::move(h), value.value.type().HashValue(ref));
    }
    return h;
  }

 private:
  core::Params<type::QualType> types_;
  ir::CompleteResultBuffer buffer_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_BOUND_PARAMETERS_H
