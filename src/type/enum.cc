#include "enum.h"

namespace type {
Enum::Enum(const std::string &name, std::vector<std::string> members,
           bool is_enum)
    : bound_name(name), members_(std::move(members)), is_enum_(is_enum) {
  auto num_members = members_.size();
  for (size_t i = 0; i < num_members; ++i) {
    int_values[members_[i]] = is_enum ? i : (1 << i);
  }
}

size_t Enum::IntValueOrFail(const std::string &str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? std::numeric_limits<size_t>::max()
                                    : iter->second;
}

IR::Val Enum::EmitInitialValue() const {
  // TODO enums don't have intial values. flags do
  return IR::Val::Enum(this, 0);
}

IR::Val Enum::EmitLiteral(const std::string &member_name) const {
  return IR::Val::Enum(this, int_values AT(member_name));
}
} // namespace type
