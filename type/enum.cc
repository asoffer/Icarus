#include "type/enum.h"


#include "absl/container/flat_hash_set.h"
#include "ir/addr.h"
#include "ir/cmd.h"
#include "ir/register.h"
#include "ir/val.h"

struct Module;
struct Context;

namespace type {

void Enum::EmitInit(ir::Register id_reg, Context *) const {
  UNREACHABLE("Enums must be initialized");
}

std::optional<ir::EnumVal> Enum::Get(const std::string &str) const {
  if (auto iter = vals_.find(str); iter != vals_.end()) { return iter->second; }
  return std::nullopt;
}

Typed<ir::EnumVal, Enum> Enum::EmitLiteral(
    std::string const &member_name) const {
  return Typed<ir::EnumVal, Enum>(vals_.at(member_name), this);
}

void Enum::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                          ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::EnumVal>(0), to);
}

void Enum::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                          ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.get<ir::EnumVal>(0), to);
}

void Enum::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  NOT_YET();
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
layout::Bytes Enum::bytes(layout::Arch const &a) const {
  return layout::Bytes{8};
}

// TODO make this the smallest size that fits.
layout::Alignment Enum::alignment(layout::Arch const &a) const {
  return layout::Alignment{8};
}

Cmp Enum::Comparator() const { return Cmp::Equality; }

}  // namespace type
