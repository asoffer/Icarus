#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include "type/typed_value.h"

namespace AST {
struct Expression;

struct OverloadSet : public base::vector<type::Typed<Expression*>> {
  OverloadSet() = default;
  template <typename T>
  OverloadSet(base::vector<type::Typed<T*>> const& xs) {
    for (auto const& x : xs) { emplace_back(x); }
  }
};
}  // namespace AST

#endif  // ICARUS_AST_OVERLOAD_SET_H
