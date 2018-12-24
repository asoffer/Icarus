#include "type/function.h"

#include "ir/val.h"

namespace type {

void GenericFunction::EmitAssign(const Type *from_type, ir::Val const &from,
                          ir::RegisterOr<ir::Addr> to, Context *ctx) const {}
void GenericFunction::EmitInit(ir::Register reg, Context *ctx) const {}
void GenericFunction::EmitDestroy(ir::Register reg, Context *ctx) const {}
ir::Val GenericFunction::PrepareArgument(const Type *t, const ir::Val &val,
                                  Context *ctx) const {
  NOT_YET();
}
void GenericFunction::EmitRepr(ir::Val const &id_val, Context *ctx) const {}

}  // namespace type