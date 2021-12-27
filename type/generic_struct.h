#ifndef ICARUS_TYPE_GENERIC_STRUCT_H
#define ICARUS_TYPE_GENERIC_STRUCT_H

#include <ostream>
#include <sstream>
#include <vector>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "core/params.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "type/callable.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct GenericStruct : ReturningType {
  GenericStruct(core::Params<QualType> in)
      : ReturningType(LegacyType::Flags{.is_default_initializable = 0,
                                        .is_copyable              = 1,
                                        .is_movable               = 1,
                                        .has_destructor           = 0},
                      std::move(in), {type::Type_}) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  bool is_big() const override { return false; }
  void ShowValue(std::ostream &, ir::CompleteResultRef const &) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return Completeness::Complete; }
};

GenericStruct const *GenStruct(core::Params<QualType> const &in);

struct GenericStructTypeInstruction
    : base::Extend<GenericStructTypeInstruction>::With<
          base::BaseSerializeExtension, base::BaseTraverseExtension> {
  Type Resolve() const {
    core::Params<QualType> params;
    params.reserve(inputs.size());
    for (auto const &[name, t] : inputs) {
      params.append(
          name.empty()
              ? core::AnonymousParam(QualType::NonConstant(t.value()))
              : core::Param<type::QualType>(std::move(name),
                                            QualType::NonConstant(t.value())));
    }

    return GenStruct(params);
  }

  friend std::ostream &operator<<(std::ostream &os,
                                  GenericStructTypeInstruction const &f) {
    os << f.result << " = struct (";
    std::string_view separator = "";
    for (auto const &[name, type] : f.inputs) {
      os << std::exchange(separator, ", ") << name << ": " << type;
    }
    return os << ")";
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  std::vector<std::pair<std::string, ir::RegOr<Type>>> inputs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_STRUCT_H
