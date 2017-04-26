#include "type.h"

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

static size_t BytesAndAlignment(const Enum *e) {
  auto num_members = e->members.size();
  if (num_members < (1ul << (1ul << 3ul))) { return 1; }
  if (num_members < (1ul << (1ul << 4ul))) { return 2; }
  if (num_members < (1ul << (1ul << 5ul))) { return 4; }
  // TODO Error message if you have too many members
  return 8;
}

size_t Enum::bytes() const { return BytesAndAlignment(this); }
size_t Enum::alignment() const { return BytesAndAlignment(this); }

Type *Enum::ProxyType() const {
  switch (BytesAndAlignment(this)) {
  case 1: return Char;
  case 2: return U16;
  case 4: return U32;
  case 8: return Uint;
  default: UNREACHABLE;
  }
}

IR::Val Enum::EmitInitialValue() const {
  switch (BytesAndAlignment(this)) {
  case 1: return IR::Val::Char('\0');
  case 2: return IR::Val::U16((uint16_t)0);
  case 4: return IR::Val::U32((uint32_t)0);
  case 8: return IR::Val::Uint(0ul);
  default: UNREACHABLE;
  }
}

IR::Val Enum::EmitLiteral(const std::string &member_name) const {
  switch (BytesAndAlignment(this)) {
  case 1: return IR::Val::Char((char)int_values.at(member_name));
  case 2: return IR::Val::U16((uint16_t)int_values.at(member_name));
  case 4: return IR::Val::U32((uint32_t)int_values.at(member_name));
  case 8: return IR::Val::Uint((size_t)int_values.at(member_name));
  default: UNREACHABLE;
  }
}

std::string Enum::to_string() const { return bound_name; }
bool Enum::private_has_vars() { return false; }

