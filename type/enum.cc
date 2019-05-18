#include "type/enum.h"


#include "absl/container/flat_hash_set.h"

struct Module;
struct Context;

namespace type {

std::optional<ir::EnumVal> Enum::Get(const std::string &str) const {
  if (auto iter = vals_.find(str); iter != vals_.end()) { return iter->second; }
  return std::nullopt;
}

// TODO is integral and same size?
bool Enum::ReinterpretAs(Type const *t) const { return t == this; }

Typed<ir::EnumVal, Enum> Enum::EmitLiteral(
    std::string const &member_name) const {
  return Typed<ir::EnumVal, Enum>(vals_.at(member_name), this);
}

void Enum::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(mod_);
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

// TODO make this the smallest size that fits.
core::Bytes Enum::bytes(core::Arch const &a) const {
  return core::Bytes{8};
}

// TODO make this the smallest size that fits.
core::Alignment Enum::alignment(core::Arch const &a) const {
  return core::Alignment{8};
}

Cmp Enum::Comparator() const { return Cmp::Equality; }

}  // namespace type
