#ifndef ICARUS_AST_BOUND_CONSTANTS_H
#define ICARUS_AST_BOUND_CONSTANTS_H

#include <sstream>

#include "base/container/map.h"
#include "base/string.h"
#include "ir/val.h"

namespace ast {
struct Declaration;

struct BoundConstants {
  base::map<Declaration const*, ir::Val> constants_;

  // TODO blah.
  std::string to_string() const {
    std::stringstream ss;
    ss << "{\n";
    for ( auto const&[decl,val]: constants_) {
      ss << reinterpret_cast<uintptr_t>(decl) << ": " << val.to_string()
         << "\n";
    }

    ss << "}";
    return ss.str();
  }
};
}  // namespace ast

namespace std {
template <>
struct less<ast::BoundConstants> {
  bool operator()(const ast::BoundConstants& lhs,
                  const ast::BoundConstants& rhs) const {
    return (lhs.constants_ < rhs.constants_);
  }
};
}  // namespace std

#endif  // ICARUS_AST_BOUND_CONSTANTS_H
