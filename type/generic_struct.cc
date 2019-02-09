#include "type/generic_struct.h"

namespace type {
Type const *Generic = new GenericFunction;

void GenericStruct::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                                   ir::RegisterOr<ir::Addr> to,
                                   Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                                   ir::RegisterOr<ir::Addr> to,
                                   Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitInit(ir::Register id_reg, Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitDestroy(ir::Register reg, Context *ctx) const {
  UNREACHABLE();
}

GenericStruct *GenStruct(::Scope const* scope, std::vector<Type const *> ts) {
  return new GenericStruct(scope, std::move(ts));
}

void GenericStruct::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

void GenericStruct::EmitRepr(ir::Val const &val, Context *ctx) const {
  UNREACHABLE();
}

void GenericStruct::WriteTo(std::string *result) const {
  result->append("[");
  if (!deps_.empty()) {
    deps_[0]->WriteTo(result);
    for (size_t i = 1; i < deps_.size(); ++i) {
      result->append(", ");
      deps_[i]->WriteTo(result);
    }
  }
  result->append("; struct]");
}

bool GenericStruct::IsCopyable() const { UNREACHABLE(); }
bool GenericStruct::IsMovable() const { UNREACHABLE(); }

}  // namespace type
