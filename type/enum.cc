#include "type/enum.h"

#include <unordered_set>

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
    std::unordered_set<::Module const *> *modules) const {
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

Cmp Enum::Comparator() const { return Cmp::Equality; }

}  // namespace type
