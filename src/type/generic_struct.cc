#include "type/generic_struct.h"

namespace type {
void GenericStruct::EmitAssign(Type const *from_type, ir::Val const &from,
                        ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitInit(ir::Register id_reg, Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitDestroy(ir::Register reg, Context *ctx) const {}

GenericStruct *GenStruct(::Scope const* scope, base::vector<Type const *> ts) {
  return new GenericStruct(scope, std::move(ts));
}

void GenericStruct::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

}  // namespace type
