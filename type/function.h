#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

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
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Function : ReturningType {
  Function(core::Params<QualType> in, std::vector<Type> out, bool eager)
      : ReturningType(IndexOf<Function>(),
                      LegacyType::Flags{.is_default_initializable = 0,
                                        .is_copyable              = 1,
                                        .is_movable               = 1,
                                        .has_destructor           = 0},
                      std::move(in), std::move(out), eager) {}

  bool is_big() const override { return false; }
  void ShowValue(std::ostream &, ir::CompleteResultRef const &) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Completeness completeness() const override { return Completeness::Complete; }

  template <typename H>
  friend H AbslHashValue(H h, Function const &f) {
    return H::combine(std::move(h), f.eager(), f.params(), f.return_types());
  }

  friend bool operator==(Function const &lhs, Function const &rhs);

  friend bool operator!=(Function const &lhs, Function const &rhs) {
    return not(lhs == rhs);
  }
};

Function const *Func(core::Params<QualType> in, std::vector<Type> out);
Function const *EagerFunc(core::Params<QualType> in, std::vector<Type> out);

struct FunctionTypeInstruction
    : base::Extend<FunctionTypeInstruction>::With<base::BaseSerializeExtension,
                                                  base::BaseTraverseExtension> {
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

    std::vector<Type> outputs_types;
    outputs_types.reserve(outputs.size());
    for (auto const &t : outputs) { outputs_types.push_back(t.value()); }

    return Func(std::move(params), std::move(outputs_types));
  }

  friend std::ostream &operator<<(std::ostream &os,
                                  FunctionTypeInstruction const &f) {
    os << f.result << " = (";
    char const *separator = "";
    for (auto const &[name, type] : f.inputs) {
      os << std::exchange(separator, ", ") << name << ": " << type;
    }
    os << ") -> (";
    for (auto const &output : f.outputs) {
      os << std::exchange(separator, ", ") << output;
    }
    return os << ")";
  }

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return std::move(ss).str();
  }

  std::vector<std::pair<std::string, ir::RegOr<Type>>> inputs;
  std::vector<ir::RegOr<Type>> outputs;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
