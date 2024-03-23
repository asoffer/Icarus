#ifndef ICARUS_TYPE_PARAMETERS_H
#define ICARUS_TYPE_PARAMETERS_H

#include "common/identifier.h"
#include "type/type.h"

namespace ic::type {

Parameter Param(Identifier name, Type t) {
  return {.name = name, .type = t.index()};
};
Parameter Param(Type t) { return {.type = t.index()}; };

// Represents a set of parameters to an invocable type.
struct ParametersType : Type {
  explicit ParametersType() = default;

  using Parameter = ::ic::type::Parameter;

  size_t size() const;
  Parameter operator[](size_t index) const;

  std::vector<Type> types() const;

  friend void NthPrint(auto& p, auto& fmt, ParametersType params) {
    std::string_view separator = "";
    for (size_t i = 0; i < params.size(); ++i) {
      auto param = params[i];
      p.write(std::exchange(separator, ", "));
      fmt(p, param);
    }
  }

 private:
  friend Type;
  friend struct FunctionType;
  friend ParametersType Parameters(std::span<Parameter const>);
  explicit constexpr ParametersType(uint32_t n)
      : Type(Type::Kind::Parameters, n) {}
};

ParametersType Parameters(std::span<Parameter const> p);
ParametersType Parameters(std::initializer_list<Parameter> p);

}  // namespace ic::type

#endif  // ICARUS_TYPE_PARAMETERS_H
