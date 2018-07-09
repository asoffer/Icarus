#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include "type.h"

namespace type {
struct Enum : public type::Type {
  TYPE_FNS(Enum);
  Enum(const std::string &name, base::vector<std::string> members);

  size_t IntValueOrFail(const std::string &str) const;
  IR::Val EmitLiteral(const std::string &member_name) const;

  // TODO combine "members" and "int_values" to save the double allocation of
  // strings.
  std::string bound_name;
  base::vector<std::string> members_;
  base::unordered_map<std::string, size_t> int_values;
};
} // namespace type
#endif // ICARUS_TYPE_ENUM_H
