#include "type/flags.h"

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
  for (auto const &[s, v] : members) {
    if (v.has_value()) { continue; }
    std::random_device rd;
    std::uniform_int_distribution<int> dist(0, 31);
    int32_t x;
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

bool Flags::ReinterpretAs(Type const *t) const { return t == this; }

std::optional<ir::FlagsVal> Flags::Get(const std::string &str) const {
  if (auto iter = vals_.find(str); iter != vals_.end()) { return iter->second; }
  return std::nullopt;
}

Typed<ir::FlagsVal, Flags> Flags::EmitLiteral(
    std::string const &member_name) const {
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

Cmp Flags::Comparator() const { return Cmp::Order; }

}  // namespace type
