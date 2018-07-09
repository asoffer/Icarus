#ifndef ICARUS_AST_BOUND_CONSTANTS_H
#define ICARUS_AST_BOUND_CONSTANTS_H

#include "base/container/map.h"
#include "base/string.h"
#include "ir/val.h"

namespace AST {
struct Declaration;

struct BoundConstants {
  base::map<std::string, IR::Val> constants_;

  base::map<Declaration*, const type::Type*> interfaces_;

  // TODO blah.
  std::string to_string() const {
    return "constants: " + base::internal::stringify(constants_) +
           "\ninterfaces: " + base::internal::stringify(interfaces_);
  }
};


inline const IR::Val* find(const BoundConstants* bc, const std::string& str) {
  if (!bc) { return nullptr; }
  if (auto iter = bc->constants_.find(str); iter != bc->constants_.end()) { return &iter->second; }
  return nullptr;
}
} // namespace AST

namespace std {
  template<>
    struct less<AST::BoundConstants> {
      bool operator()(const AST::BoundConstants& lhs, const AST::BoundConstants& rhs) {
        if (lhs.constants_ < rhs.constants_) { return true; }
        if (rhs.constants_ < lhs.constants_) { return false; }
        return lhs.interfaces_ < rhs.interfaces_;
      }
    };
}

#endif // ICARUS_AST_BOUND_CONSTANTS_H
