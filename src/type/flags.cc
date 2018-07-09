#include "flags.h"

namespace type {
Flags::Flags(const std::string &name, base::vector<std::string> members)
    : bound_name(name), members_(std::move(members)) {
  auto num_members = members_.size();
  for (size_t i = 0; i < num_members; ++i) { int_values[members_[i]] = 1 << i; }
}

size_t Flags::IntValueOrFail(const std::string &str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? std::numeric_limits<size_t>::max()
                                    : iter->second;
}

IR::Val Flags::EmitLiteral(const std::string &member_name) const {
  return IR::Val::Flags(this, int_values.at(member_name));
}
} // namespace type
