#include "type/generic_struct.h"

namespace type {

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

core::Bytes GenericStruct::bytes(core::Arch const &a) const {
  return core::Host().ptr_bytes;
}

core::Alignment GenericStruct::alignment(core::Arch const &a) const {
  return core::Host().ptr_alignment;
}

Cmp GenericStruct::Comparator() const { return Cmp::None; }

bool GenericStruct::ReinterpretAs(Type const *t) const { return t == this; }

}  // namespace type
