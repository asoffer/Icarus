#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include "type.h"

namespace type {
struct Enum : public type::Type {
  TYPE_FNS(Enum);
  Enum(const std::string &name, std::vector<std::string> members, bool is_enum);

  size_t IntValueOrFail(const std::string &str) const;
  IR::Val EmitLiteral(const std::string &member_name) const;

  // TODO combine "members" and "int_values" to save the double allocation of
  // strings.
  std::string bound_name;
  std::vector<std::string> members_;
  std::unordered_map<std::string, size_t> int_values;
  bool is_enum_;
};
} // namespace type
#endif // ICARUS_TYPE_ENUM_H
