#include "type.h"
#include "../architecture.h"

extern FileType file_type;

Enum::Enum(const std::string &name, const std::vector<std::string> &members)
    : bound_name(name), members(members) {
  auto num_members = members.size();
  for (size_t i = 0; i < num_members; ++i) { int_values[members[i]] = i; }
}

size_t Enum::IndexOrFail(const std::string &str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? FAIL : iter->second;
}

IR::Val Enum::EmitInitialValue() const {
  // TODO Is const correctness worth the pain?
  return IR::Val::Enum(const_cast<Enum *>(this), 0);
}

IR::Val Enum::EmitLiteral(const std::string &member_name) const {
  // TODO Is const correctness worth the pain?
  return IR::Val::Enum(const_cast<Enum *>(this), int_values.at(member_name));
}

std::string Enum::to_string() const { return bound_name; }
