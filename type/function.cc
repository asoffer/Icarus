#include "type/function.h"

#include "absl/hash/hash.h"
#include "common/constant/manifest.h"
#include "common/internal/functions.h"
#include "nth/utility/no_destructor.h"

namespace ic::type {

struct FunctionInsertionType {
  type::ParametersType parameters;
  std::span<type::Type const> returns;
  type::Evaluation evaluation;
  size_t index;

  operator size_t() const { return index; }
};

FunctionType Function(ParametersType pt, std::span<Type const> rets,
                      Evaluation e) {
  size_t position = ConstantManifest::Global().size();

  // TODO: Double-lookup because we don't have heterogeneous insertion.
  auto iter = internal_common::Functions().find(
      FunctionInsertionType{pt, rets, e, position});

  if (iter == internal_common::Functions().end()) {
    InsertIntoGlobalConstantManifest(ConstantCategory::FunctionType,
                                     rets.size());
    InsertIntoGlobalConstantManifest(ConstantCategory::Followup, pt.index());
    InsertIntoGlobalConstantManifest(ConstantCategory::Followup,
                                     static_cast<int>(e));
    for (Type ret : rets) {
      InsertIntoGlobalConstantManifest(ConstantCategory::Followup, ret.index());
    }

    iter = internal_common::Functions().insert(position).first;
  }

  return FunctionType(*iter);
}

FunctionType Function(ParametersType pt, std::initializer_list<Type> rets,
                      Evaluation e) {
  return Function(pt, std::span(rets.begin(), rets.end()), e);
}

ParametersType FunctionType::parameters() const {
  return ParametersType(ConstantManifest::Global()[index() + 1].value());
}

Evaluation FunctionType::evaluation() const {
  return static_cast<Evaluation>(
      ConstantManifest::Global()[index() + 2].value());
}

ReturnsType FunctionType::returns() const {
  return ReturnsType(ConstantManifest::Global()[index() + 3].value(),
                     ConstantManifest::Global()[index()].value());
}

Type ReturnsType::operator[](size_t index) const {
  NTH_REQUIRE((v.harden), index < size());
  return Type(Type::from_index,
              ConstantManifest::Global()[index_ + index].value());
}

ReturnsType::iterator ReturnsType::begin() const {
  return iterator(&ConstantManifest::Global()[index_]);
}

ReturnsType::iterator ReturnsType::end() const {
  return iterator(&ConstantManifest::Global()[index_ + size_]);
}

}  // namespace ic::type
