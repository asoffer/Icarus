#ifndef ICARUS_TYPE_ENUM_H
#define ICARUS_TYPE_ENUM_H

#include <random>
#include <unordered_set>
#include <optional>

#include "base/container/unordered_map.h"
#include "ir/register.h"
#include "type.h"
#include "typed_value.h"

namespace type {
struct Enum : public type::Type {
  TYPE_FNS(Enum);

  Enum(base::unordered_map<std::string, std::optional<i32>> const& members) {
    std::unordered_set<i32> taken;
    for (auto const & [ s, v ] : members) {
      if (v.has_value()) {
        vals_.emplace(s, ir::EnumVal(*v));
        members_.emplace(*v, s);
      }
      taken.insert(*v);
    }
    // TODO we can highly optimize this in a number of ways. One simple thing is
    // removing members as we used them above.
    for (auto const & [ s, v ] : members) {
      if (v.has_value()) { continue; }
      std::random_device rd;
      std::uniform_int_distribution<int> dist(
          std::numeric_limits<i32>::lowest(), std::numeric_limits<i32>::max());
      i32 x;
      {
      try_again:
        x            = dist(rd);
        bool success = taken.insert(x).second;
        if (!success) { goto try_again; }
      }
      vals_.emplace(s, ir::EnumVal(x));
      members_.emplace(x, s);
    }
  }

  bool IsDefaultInitializable() const override { return false; }

  std::optional<ir::EnumVal> Get(const std::string& str) const {
    if (auto iter = vals_.find(str); iter != vals_.end()) {
      return iter->second;
    }
    return std::nullopt;
  }

  Typed<ir::EnumVal, Enum> EmitLiteral(std::string const& member_name) const {
    return Typed<ir::EnumVal, Enum>(vals_.at(member_name), this);
  }

  // TODO privatize
  base::unordered_map<i32, std::string> members_;

 private:
  // TODO combine these into a single bidirectional map?
  base::unordered_map<std::string, ir::EnumVal> vals_;
};
}  // namespace type

#endif  // ICARUS_TYPE_ENUM_H
