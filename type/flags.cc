#include "type/flags.h"

#include <unordered_set>

#include "ir/addr.h"
#include "ir/cmd.h"
#include "ir/register.h"
#include "ir/val.h"

struct Module;
struct Context;

namespace type {

void Flags::EmitInit(ir::Register id_reg, Context *) const {
  ir::Store(ir::FlagsVal{0}, id_reg);
}

void Flags::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                       ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::FlagsVal>(), to);
}

void Flags::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                       ir::RegisterOr<ir::Addr> to, Context *) const {
  ASSERT(this == from_type);
  ir::Store(from.reg_or<ir::FlagsVal>(), to);
}

void Flags::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

void Flags::EmitRepr(ir::Val const &val, Context *) const {
  ir::Print(val.reg_or<ir::FlagsVal>(), this);
}

void Flags::WriteTo(std::string *result) const {
  result->append("flags.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

}  // namespace type
