#include "type/opaque.h"

#include "base/debug.h"
#include "ir/val.h"

namespace type {
void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

void Opaque::EmitCopyAssign(const Type *from_type, ir::Results const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  UNREACHABLE();
}

void Opaque::EmitMoveAssign(const Type *from_type, ir::Results const &from,
                            ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  UNREACHABLE();
}

void Opaque::EmitInit(ir::Register reg, Context *ctx) const { UNREACHABLE(); }

void Opaque::EmitDestroy(ir::Register reg, Context *ctx) const {
  UNREACHABLE();
}

ir::Results Opaque::PrepareArgument(const Type *t, const ir::Results &val,
                                    Context *ctx) const {
  UNREACHABLE();
}

void Opaque::EmitRepr(ir::Results const &id_val, Context *ctx) const {
  UNREACHABLE();
}

void Opaque::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(mod_);
}

Cmp Opaque::Comparator() const { UNREACHABLE(); }

}  // namespace type
