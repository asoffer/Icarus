#include "type/parameters.h"

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/hash/hash.h"
#include "common/constant/manifest.h"
#include "common/internal/parameters.h"
#include "nth/utility/no_destructor.h"

namespace ic::type {

size_t ParametersType::size() const {
  return ConstantManifest::Global()[index()].value();
}

Parameter ParametersType::operator[](size_t index) const {
  NTH_REQUIRE((v.harden), index < size());
  return Parameter{
      .name = Identifier::FromRepresentation(
          ConstantManifest::Global()[this->index() + index * 2 + 1].value()),
      .type = Type(
          Type::from_index,
          ConstantManifest::Global()[this->index() + index * 2 + 2].value()),

  };
}

std::vector<Type> ParametersType::types() const {
  std::vector<Type> result;
  for (size_t i = 0; i < size(); ++i) { result.push_back(operator[](i).type); }
  return result;
}

ParametersType Parameters(std::span<Parameter const> p) {
  size_t position = ConstantManifest::Global().size();
  auto iter       = internal_common::Parameters().find(ParameterInsertionType{p, position});
  if (iter == internal_common::Parameters().end()) {
    NTH_LOG("---");
    InsertIntoGlobalConstantManifest(ConstantCategory::ParametersType,
                                     p.size());
    for (auto parameter : p) {
      InsertIntoGlobalConstantManifest(
          ConstantCategory::Followup,
          Identifier::ToRepresentation(parameter.name));
      InsertIntoGlobalConstantManifest(ConstantCategory::Followup,
                                       parameter.type.index());
    }

    // TODO: Double-lookup because we don't have heterogeneous insertion.
     NTH_LOG("---");
   iter = internal_common::Parameters().insert(position).first;
  }

  return ParametersType(*iter);
}

ParametersType Parameters(std::initializer_list<Parameter> p) {
  return Parameters(std::span<Parameter const>(p.begin(), p.end()));
}

}  // namespace ic::type
