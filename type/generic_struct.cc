#include "type/generic_struct.h"

namespace type {

GenericStruct *GenStruct(core::Scope const* scope, std::vector<Type const *> ts) {
  return new GenericStruct(scope, std::move(ts));
}

void GenericStruct::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  modules->insert(defining_module());
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

core::Bytes GenericStruct::bytes(core::Arch const &a) const {
  return core::Host().ptr_bytes;
}

core::Alignment GenericStruct::alignment(core::Arch const &a) const {
  return core::Host().ptr_alignment;
}

bool GenericStruct::ReinterpretAs(Type const *t) const { return t == this; }

}  // namespace type
