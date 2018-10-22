#ifndef ICARUS_AST_BOUND_CONSTANTS_H
#define ICARUS_AST_BOUND_CONSTANTS_H

#include "base/container/map.h"
#include "base/string.h"
#include "ir/val.h"

namespace AST {
struct Declaration;

struct BoundConstants {
  base::map<Declaration*, IR::Val> constants_;

  // TODO blah.
  std::string to_string() const {
    return base::internal::stringify(constants_);
  }

  bool exntends(BoundConstants const& bc) const {
    if (bc.constants_.size() > constants_.size()) { return false; }
    auto this_iter = constants_.begin();
    auto bc_iter   = bc.constants_.begin();
    while (true) {
      if (this_iter == constants_.end()) {
        return bc_iter == bc.constants_.end();
      }
      if (bc_iter == bc.constants_.end()) { return true; }

      if (this_iter->first < bc_iter->first) {
        ++this_iter;
        continue;
      } else if (this_iter->first == bc_iter->first) {
        if (this_iter->second != bc_iter->second) { return false; }
        ++this_iter;
        ++bc_iter;
        continue;
      } else {
        return false;
      }
    }
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
