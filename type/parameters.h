#ifndef ICARUS_TYPE_PARAMETERS_H
#define ICARUS_TYPE_PARAMETERS_H

#include <utility>
#include <vector>

#include "type/basic.h"

namespace ic::type {

// Represents a set of parameters to an invocable type.
struct ParametersType : internal_type::BasicType {
  struct Parameter {
    uint64_t name;
    Type type;

    friend bool operator==(Parameter, Parameter) = default;

    template <typename H>
    friend H AbslHashValue(H h, Parameter p) {
      return H::combine(std::move(h), p.name, p.type);
    }

    friend void NthPrint(auto& p, auto& fmt, Parameter param) {
      fmt(p, param.name);
      p.write(": ");
      fmt(p, param.type);
    }
  };

  size_t size() const;
  std::vector<Parameter> const& operator*() const;

  friend void NthPrint(auto& p, auto& fmt, ParametersType params) {
    std::string_view separator = "";
    for (auto const& param : *params) {
      p.write(std::exchange(separator, ", "));
      fmt(p, param);
    }
  }

 private:
  friend Type;
  friend ParametersType Parameters(std::vector<ParametersType::Parameter>&&);
  friend ParametersType Parameters(
      std::vector<ParametersType::Parameter> const&);

  explicit ParametersType() = default;
  explicit constexpr ParametersType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Parameters, n) {}
};

ParametersType Parameters(std::vector<ParametersType::Parameter>&& p);
ParametersType Parameters(std::vector<ParametersType::Parameter> const& p);


}  // namespace ic::type

#endif // ICARUS_TYPE_PARAMETERS_H
