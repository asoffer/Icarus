#include "type/enum.h"


#include "absl/container/flat_hash_set.h"
#include "ir/addr.h"
#include "ir/cmd.h"
#include "ir/register.h"

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

// TODO print something friendlier
void Enum::EmitRepr(ir::Results const &val, Context *) const {
  ir::Print(val.get<ir::EnumVal>(0), this);
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

ir::Results Enum::PrepareArgument(Type const *from, ir::Results const &val,
                                  Context *ctx) const {
  ASSERT(from == this);
  return val;
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
