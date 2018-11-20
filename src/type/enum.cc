#include "type/enum.h"
#include "ir/val.h"

namespace type {
Enum::Enum(base::vector<std::string> members) : members_(std::move(members)) {
  auto num_members = members_.size();
  for (size_t i = 0; i < num_members; ++i) { int_values[members_[i]] = i; }
}

size_t Enum::IntValueOrFail(const std::string &str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? std::numeric_limits<size_t>::max()
                                    : iter->second;
}

ir::Val Enum::EmitLiteral(const std::string &member_name) const {
  return ir::Val::Enum(this, int_values.at(member_name));
}
}  // namespace type
