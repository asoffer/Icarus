#ifndef ICARUS_COMMON_INTERNAL_FUNCTIONS_H
#define ICARUS_COMMON_INTERNAL_FUNCTIONS_H

#include <concepts>
#include <string>

#include "absl/container/flat_hash_set.h"
#include "absl/hash/hash.h"
#include "common/constant/component.h"

namespace ic::type {
struct FunctionInsertionType;
}  // namespace ic::type

namespace ic::internal_common {

struct FunctionHash {
  using is_transparent = void;

  explicit FunctionHash(std::vector<ConstantComponent> const& components
                            NTH_ATTRIBUTE(lifetimebound))
      : components_(components) {}
  [[nodiscard]] size_t operator()(size_t index) const {
    size_t ret_count = components_[index].value();
    size_t result    = 0;
    result           = absl::HashOf(components_[index + 1].value());
    for (size_t i = 3; i < ret_count + 3; ++i) {
      result = absl::HashOf(result, components_[index + i].value());
    }
    return result;
  }

  [[nodiscard]] size_t operator()(
      std::same_as<type::FunctionInsertionType> auto fit) const {
    size_t result = absl::HashOf(fit.parameters, fit.evaluation);
    for (auto r : fit.returns) { result = absl::HashOf(result, r); }
    return result;
  }

 private:
  std::vector<ConstantComponent> const& components_;
};

struct FunctionEq {
  using is_transparent = void;

  explicit FunctionEq(std::vector<ConstantComponent> const& components
                          NTH_ATTRIBUTE(lifetimebound))
      : components_(components) {}

  [[nodiscard]] bool operator()(size_t lhs, size_t rhs) const {
    return lhs == rhs;
  }

  [[nodiscard]] bool operator()(
      size_t lhs, std::same_as<type::FunctionInsertionType> auto rhs) const {
    return operator()(rhs, lhs);
  }

  [[nodiscard]] bool operator()(
      std::same_as<type::FunctionInsertionType> auto lhs, size_t rhs) const {
    if (lhs.parameters.index() != components_[rhs + 1].value()) {
      return false;
    }
    if (static_cast<int>(lhs.evaluation) != components_[rhs + 2].value()) {
      return false;
    }
    size_t ret_count = components_[rhs].value();
    if (lhs.returns.size() != ret_count) { return false; }
    for (size_t i = 0; i < ret_count; ++i) {
      if (lhs.returns[i].index() != components_[rhs + i + 3].value()) {
        return false;
      }
    }
    return true;
  }

 private:
  std::vector<ConstantComponent> const& components_;
};

void InitializeFunctions(std::vector<ConstantComponent> const& components);
absl::flat_hash_set<size_t, FunctionHash, FunctionEq>& Functions();

}  // namespace ic::internal_common

#endif  // ICARUS_COMMON_INTERNAL_FUNCTIONS_H
