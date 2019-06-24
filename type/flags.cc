#include "type/flags.h"

#include "absl/random/distributions.h"
#include "absl/random/random.h"

struct Module;
struct Context;

namespace type {

Flags::Flags(
    absl::flat_hash_map<std::string, std::optional<int32_t>> const &members) {
  absl::flat_hash_set<int32_t> taken;
  for (auto const &[s, v] : members) {
    if (v.has_value()) {
      vals_.emplace(s, ir::FlagsVal(size_t{1} << *v));
      members_.emplace(size_t{1} << *v, s);
      All |= (size_t{1} << *v);
    }
    taken.insert(*v);
  }
  // TODO we can highly optimize this in a number of ways. One simple thing is
  // removing members as we used them above.
  absl::BitGen gen;
  for (auto const &[s, v] : members) {
    if (v.has_value()) { continue; }
    int32_t x;
    {
    try_again:
      x            = absl::Uniform(absl::IntervalClosedOpen, gen, 0, 32);
      bool success = taken.insert(x).second;
      if (!success) { goto try_again; }
    }
    vals_.emplace(s, ir::FlagsVal(size_t{1} << x));
    All |= (size_t{1} << x);
    members_.emplace(size_t{1} << x, s);
  }
}

std::optional<ir::FlagsVal> Flags::Get(std::string_view name) const {
  if (auto iter = vals_.find(name); iter != vals_.end()) { return iter->second; }
  return std::nullopt;
}

Typed<ir::FlagsVal, Flags> Flags::EmitLiteral(
    std::string_view member_name) const {
  return Typed<ir::FlagsVal, Flags>(vals_.at(member_name), this);
}

void Flags::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(mod_);
}

void Flags::WriteTo(std::string *result) const {
  result->append("flags.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

// TODO make this the smallest size that fits.
core::Bytes Flags::bytes(core::Arch const &a) const {
  return core::Bytes{8};
}

// TODO make this the smallest size that fits.
core::Alignment Flags::alignment(core::Arch const &a) const {
  return core::Alignment{8};
}

}  // namespace type
