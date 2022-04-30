#ifndef ICARUS_TYPE_FUNCTION_INSTRUCTIONS_H
#define ICARUS_TYPE_FUNCTION_INSTRUCTIONS_H

#include <ostream>
#include <sstream>
#include <vector>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "core/parameters.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "type/callable.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {
struct FunctionTypeInstruction
    : base::Extend<FunctionTypeInstruction>::With<base::BaseSerializeExtension,
                                                  base::BaseTraverseExtension> {
  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   FunctionTypeInstruction const &inst) {
    core::Parameters<QualType> params;
    params.reserve(inst.inputs.size());
    for (auto const &[name, t] : inst.inputs) {
      params.append(name.empty()
                        ? core::AnonymousParameter(QualType::NonConstant(
                              interpreter.frame().resolve(t)))
                        : core::Parameter<type::QualType>{
                              .name  = std::move(name),
                              .value = QualType::NonConstant(
                                  interpreter.frame().resolve(t)),
                          });
    }

    std::vector<Type> outputs_types;
    outputs_types.reserve(inst.outputs.size());
    for (auto const &t : inst.outputs) {
      outputs_types.push_back(interpreter.frame().resolve(t));
    }

    interpreter.frame().set(
        inst.result, Type(Func(std::move(params), std::move(outputs_types))));

    return true;
  }

  Type Resolve() const {
    core::Parameters<QualType> params;
    params.reserve(inputs.size());
    for (auto const &[name, t] : inputs) {
      params.append(
          name.empty()
              ? core::AnonymousParameter(QualType::NonConstant(t.value()))
              : core::Parameter<type::QualType>{
                    .name  = std::move(name),
                    .value = QualType::NonConstant(t.value()),
                });
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

struct IsAFunctionInstruction
    : base::Extend<IsAFunctionInstruction>::With<base::BaseSerializeExtension,
                                                 base::BaseTraverseExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = is-function %1$s";

  bool Resolve() const { return operand.value().is<Function>(); }

  ir::RegOr<type::Type> operand;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_INSTRUCTIONS_H
