#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include "base/container/unordered_map.h"
#include "ir/flags_val.h"
#include "type.h"
#include "typed_value.h"

namespace type {
struct Flags : public type::Type {
  TYPE_FNS(Flags);

  Flags(base::vector<std::string> members) : members_(std::move(members)) {
    auto num_members = members_.size();
    for (size_t i = 0; i < num_members; ++i) {
      vals_[members_[i]] = ir::FlagsVal{size_t{i} << i};
    }
  }

  std::optional<ir::FlagsVal> Get(const std::string& str) const {
    if (auto iter = vals_.find(str); iter != vals_.end()) {
      return iter->second;
    }
    return std::nullopt;
  }

  Typed<ir::FlagsVal, Flags> EmitLiteral(std::string const& member_name) const {
    return Typed<ir::FlagsVal, Flags>(vals_.at(member_name), this);
  }

  // TODO privatize
  base::vector<std::string> members_;
 private:
  // TODO combine these into a single bidirectional map?
  base::unordered_map<std::string, ir::FlagsVal> vals_;
};
}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H
