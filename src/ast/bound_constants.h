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

#endif // ICARUS_AST_BOUND_CONSTANTS_H
