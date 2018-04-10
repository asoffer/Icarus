#ifndef ICARUS_AST_BOUND_CONSTANTS_H
#define ICARUS_AST_BOUND_CONSTANTS_H

#include <map>

#include "ir/val.h"

namespace AST {
struct BoundConstants : public std::map<std::string, IR::Val> {};

inline const IR::Val* find(const BoundConstants* bc, const std::string& str) {
  if (!bc) { return nullptr; }
  if (auto iter = bc->find(str); iter != bc->end()) { return &iter->second; }
  return nullptr;
}
} // namespace AST

namespace std {
template <> struct less<AST::BoundConstants> {
  bool operator()(const AST::BoundConstants &lhs,
                  const AST::BoundConstants &rhs) const {
    if (lhs.size() < rhs.size()) { return true; }
    if (lhs.size() > rhs.size()) { return false; }
    auto lhs_iter = lhs.begin();
    auto rhs_iter = rhs.begin();
    while (lhs_iter != lhs.end()) {
      ASSERT_EQ(lhs_iter->first, rhs_iter->first);
      if (lhs_iter->second.value < rhs_iter->second.value) { return true; }
      if (lhs_iter->second.value > rhs_iter->second.value) { return false; }
      ++lhs_iter;
      ++rhs_iter;
    }

    return false;
  }
};
} // namespace std

#endif // ICARUS_AST_BOUND_CONSTANTS_H
