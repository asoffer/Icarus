#ifndef ICARUS_IR_INTERFACE_H
#define ICARUS_IR_INTERFACE_H

#include <string>
#include "base/container/vector.h"

#include "base/container/map.h"

namespace ast {
struct BlockLiteral;
}  // namespace ast

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct Interface {
  base::vector<std::string> MatchErrors(const type::Type*) const;

  // TODO would prefer unordered but constraints from Val's variant make this
  // hard.
  base::map<std::string, const type::Type*> field_map_;
};
inline bool operator==(const Interface& lhs, const Interface& rhs) {
  return lhs.field_map_ == rhs.field_map_;
}

// TODO not really comparable. just for variant? :(
inline bool operator<(const Interface& lhs, const Interface& rhs) {
  return lhs.field_map_ < rhs.field_map_;
}
}  // namespace ir

#endif  // ICARUS_IR_INTERFACE_H
