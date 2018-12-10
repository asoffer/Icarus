#include "type/generic_struct.h"

namespace type {
void GenericStruct::EmitAssign(Type const *from_type, ir::Val const &from,
                        ir::Register to, Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitInit(ir::Register id_reg, Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitDestroy(ir::Register reg, Context *ctx) const {}

GenericStruct *GenStruct(Type const *t) { return new GenericStruct(t); }
GenericStruct *GenStruct(base::vector<Type const *> ts) {
  return new GenericStruct(std::move(ts));
}

}  // namespace type
