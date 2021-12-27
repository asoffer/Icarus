#include "type/generic_struct.h"

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "base/global.h"

namespace type {

static base::Global<absl::node_hash_map<core::Params<QualType>, GenericStruct>>
    gen_structs_;

GenericStruct const *GenStruct(core::Params<QualType> const &in) {
  auto handle           = gen_structs_.lock();
  auto [iter, inserted] = handle->try_emplace(in, in);
  auto const &[ret, s]  = *iter;
  return &s;
}

void GenericStruct::WriteTo(std::string *result) const {
  result->append("struct (");
  std::string_view sep = "";
  for (auto const &param : params()) {
    result->append(sep);
    if (not param.name.empty()) {
      absl::StrAppend(result, param.name,
                      param.value.constant() ? " :: " : ": ");
    }
    param.value.type().get()->WriteTo(result);
    sep = ", ";
  }
  result->append(")");
}

core::Bytes GenericStruct::bytes(core::Arch const &a) const {
  return a.function().bytes();
}

core::Alignment GenericStruct::alignment(core::Arch const &a) const {
  return a.function().alignment();
}

void GenericStruct::ShowValue(std::ostream &os,
                              ir::CompleteResultRef const &value) const {
  // TODO: Invert the dependency on //ir/value:fn so this can be implemented
  // correctly.
  os << "<<generic-struct>>";
}

}  // namespace type
