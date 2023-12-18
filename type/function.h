#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <span>
#include <utility>
#include <vector>

#include "type/basic.h"
#include "type/parameters.h"
#include "type/type_system.pb.h"

namespace ic::type {

enum Evaluation {
  RequireCompileTime,
  PreferCompileTime,
  PreferRuntime,
  RequireRuntime,
};

struct FunctionType : internal_type::BasicType {
  explicit FunctionType() = default;

  Evaluation evaluation() const;
  ParametersType parameters() const;
  std::vector<Type> const& returns() const;

  friend void NthPrint(auto& p, auto& fmt, FunctionType f) {
    std::string_view separator = "";
    p.write("(");
    fmt(p, f.parameters());
    std::span returns = f.returns();
    p.write(returns.size() != 1 ? ") -> (" : ") -> ");
    separator = "";
    for (auto const& r : f.returns()) {
      p.write(std::exchange(separator, ", "));
      fmt(p, r);
    }
    if (returns.size() != 1) { p.write(")"); }
  }

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto&);
  friend void DeserializeTypeSystem(TypeSystemProto const&);
  friend FunctionType Function(ParametersType, std::vector<Type>&&, Evaluation);
  friend FunctionType Function(ParametersType, std::vector<Type> const&,
                               Evaluation);

  explicit constexpr FunctionType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Function, n) {}
};

FunctionType Function(ParametersType pt, std::vector<Type>&& r,
                      Evaluation e = Evaluation::PreferRuntime);
FunctionType Function(ParametersType pt, std::vector<Type> const& r,
                      Evaluation e = Evaluation::PreferRuntime);

}  // namespace ic::type

#endif  // ICARUS_TYPE_FUNCTION_H
