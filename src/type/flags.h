#ifndef ICARUS_TYPE_FLAGS_H
#define ICARUS_TYPE_FLAGS_H

#include <optional>
#include <random>
#include <unordered_set>

#include "base/container/unordered_map.h"
#include "ir/flags_val.h"
#include "type.h"
#include "typed_value.h"

namespace type {
struct Flags : public type::Type {
  TYPE_FNS(Flags);

  Flags(base::unordered_map<std::string, std::optional<i32>> const& members) {
    std::unordered_set<i32> taken;
    for (auto const & [ s, v ] : members) {
      if (v.has_value()) {
        vals_.emplace(s, ir::FlagsVal(size_t{1} << *v));
        members_.emplace(size_t{1} << *v, s);
        All |= (size_t{1} << *v);
      }
      taken.insert(*v);
    }
    // TODO we can highly optimize this in a number of ways. One simple thing is
    // removing members as we used them above.
    for (auto const & [ s, v ] : members) {
      if (v.has_value()) { continue; }
      std::random_device rd;
      std::uniform_int_distribution<int> dist(0, 31);
      i32 x;
      {
      try_again:
        x            = dist(rd);
        bool success = taken.insert(x).second;
        if (!success) { goto try_again; }
      }
      vals_.emplace(s, ir::FlagsVal(size_t{1} << x));
      All |= (size_t{1} << x);
      members_.emplace(size_t{1} << x, s);
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
  base::unordered_map<size_t, std::string> members_;

  size_t All = 0;

 private:
  // TODO combine these into a single bidirectional map?
  base::unordered_map<std::string, ir::FlagsVal> vals_;
};
}  // namespace type

#endif  // ICARUS_TYPE_FLAGS_H
