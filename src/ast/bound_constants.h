#ifndef ICARUS_AST_BOUND_CONSTANTS_H
#define ICARUS_AST_BOUND_CONSTANTS_H

#include "base/container/map.h"
#include "base/string.h"
#include "ir/val.h"

namespace AST {
struct Declaration;

struct BoundConstants {
  base::map<Declaration const*, IR::Val> constants_;

  // TODO blah.
  std::string to_string() const {
    return base::internal::stringify(constants_);
  }
};
}  // namespace AST

namespace std {
template <>
struct less<AST::BoundConstants> {
  bool operator()(const AST::BoundConstants& lhs,
                  const AST::BoundConstants& rhs) const {
    return (lhs.constants_ < rhs.constants_);
  }
};
}  // namespace std

#endif  // ICARUS_AST_BOUND_CONSTANTS_H
