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

void Enum::EmitAssign(Type const *from_type, ir::Val const &from,
                      ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::EnumVal>(), to);
}

void Enum::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

// TODO print something friendlier
void Enum::EmitRepr(ir::Val const &val, Context *) const {
  ir::Print(val.reg_or<ir::EnumVal>(), this);
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

}  // namespace type
