#include "type/generic_struct.h"

namespace type {

void GenericStruct::EmitCopyAssign(Type const *from_type,
                                   ir::Results const &from,
                                   ir::RegisterOr<ir::Addr> to,
                                   Context *ctx) const {
  NOT_YET();
}

void GenericStruct::EmitMoveAssign(Type const *from_type,
                                   ir::Results const &from,
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

GenericStruct *GenStruct(core::Scope const* scope, std::vector<Type const *> ts) {
  return new GenericStruct(scope, std::move(ts));
}

void GenericStruct::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(defining_module());
}

void GenericStruct::EmitRepr(ir::Results const &val, Context *ctx) const {
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

ir::Results GenericStruct::PrepareArgument(Type const *from,
                                           ir::Results const &val,
                                           Context *ctx) const {
  NOT_YET(this, from);
}

layout::Bytes GenericStruct::bytes(layout::Arch const &a) const {
  return layout::Host().ptr_bytes;
}

layout::Alignment GenericStruct::alignment(layout::Arch const &a) const {
  return layout::Host().ptr_alignment;
}

Cmp GenericStruct::Comparator() const { return Cmp::None; }

bool GenericStruct::ReinterpretAs(Type const *t) const { return t == this; }

}  // namespace type
