#ifndef ICARUS_COMMON_INTERNAL_PARAMETERS_H
#define ICARUS_COMMON_INTERNAL_PARAMETERS_H

#include <concepts>
#include <cstdint>
#include <string>

#include "absl/container/flat_hash_set.h"
#include "absl/hash/hash.h"
#include "common/constant/component.h"
#include "common/identifier.h"

namespace ic::type {

struct Parameter {
  // TODO: Constructor.
  Identifier name;
  uint32_t type;

  friend bool operator==(Parameter, Parameter) = default;

  template <typename H>
  friend H AbslHashValue(H h, Parameter p) {
    return H::combine(std::move(h), p.name, p.type);
  }

  friend void NthPrint(auto& p, auto& fmt, Parameter param) {
    fmt(p, param.name);
    p.write(": @");
    fmt(p, param.type);
  }
};

struct ParameterInsertionType {
  std::span<type::Parameter const> parameters;
  size_t index;

  operator size_t() const { return index; }
};

}  // namespace ic::type

namespace ic::internal_common {

struct ParameterHash {
  using is_transparent = void;

  explicit ParameterHash(std::vector<ConstantComponent> const& components
                             NTH_ATTRIBUTE(lifetimebound))
      : components_(components) {}

  [[nodiscard]] size_t operator()(size_t index) const {
    NTH_REQUIRE((v.harden), index < components_.size());
    size_t size   = components_[index].value();
    size_t result = 0;
    for (size_t i = 0; i < size * 2; i += 2) {
      result = absl::HashOf(result, components_[index + i + 1].value(),
                            components_[index + i + 2].value());
    }
    return result;
  }

  [[nodiscard]] size_t operator()(type::ParameterInsertionType pit) const {
    size_t result = 0;
    for (auto p : pit.parameters) {
      result = absl::HashOf(result, p.name, p.type);
    }
    return result;
  }

 private:
  std::vector<ConstantComponent> const& components_;
};

struct ParameterEq {
  using is_transparent = void;

  explicit ParameterEq(std::vector<ConstantComponent> const& components
                           NTH_ATTRIBUTE(lifetimebound))
      : components_(components) {}

  [[nodiscard]] bool operator()(size_t lhs, size_t rhs) const {
    return lhs == rhs;
  }

  [[nodiscard]] bool operator()(size_t lhs,
                                type::ParameterInsertionType rhs) const {
    return operator()(rhs, lhs);
  }
  [[nodiscard]] bool operator()(type::ParameterInsertionType lhs,
                                size_t rhs) const {
    size_t size = components_[rhs].value();
    if (lhs.parameters.size() != size) { return false; }
    for (size_t i = 0; i < size * 2; i += 2) {
      using id_type = decltype(lhs.parameters[i / 2].name);
      if (id_type::ToRepresentation(lhs.parameters[i / 2].name) !=
          components_[rhs + i].value()) {
        return false;
      }
      if (lhs.parameters[i / 2].type != components_[rhs + i + 1].value()) {
        return false;
      }
    }
    return true;
  }

 private:
  std::vector<ConstantComponent> const& components_;
};

void InitializeParameters(std::vector<ConstantComponent> const& components);
absl::flat_hash_set<size_t, ParameterHash, ParameterEq>& Parameters();

}  // namespace ic::internal_common

#endif  // ICARUS_COMMON_INTERNAL_PARAMETERS_H
