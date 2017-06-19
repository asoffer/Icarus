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

Type *Enum::ProxyType() const {
  // TODO architecture dependence
  switch (Architecture::CompilingMachine().bytes(this)) {
  case 1: return Char;
  case 2: return U16;
  case 4: return U32;
  case 8: return Uint;
  default: UNREACHABLE;
  }
}

IR::Val Enum::EmitInitialValue() const {
  return ProxyType()->EmitInitialValue();
}

IR::Val Enum::EmitLiteral(const std::string &member_name) const {
  // TODO architecture dependence
  switch (Architecture::CompilingMachine().bytes(this)) {
  case 1: return IR::Val::Char((char)int_values.at(member_name));
  case 2: return IR::Val::U16((uint16_t)int_values.at(member_name));
  case 4: return IR::Val::U32((uint32_t)int_values.at(member_name));
  case 8: return IR::Val::Uint((size_t)int_values.at(member_name));
  default: UNREACHABLE;
  }
}

std::string Enum::to_string() const { return bound_name; }
