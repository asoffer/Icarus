#ifndef ICARUS_COMMON_CONSTANT_H
#define ICARUS_COMMON_CONSTANT_H

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "nth/base/attributes.h"
#include "nth/meta/type.h"

namespace ic {

struct ConstantComponentAppender {
  explicit constexpr ConstantComponentAppender(std::vector<uint64_t>& v)
      : v_(v) {}
  void append(uint64_t n) { v_.push_back(n); }

 private:
  std::vector<uint64_t>& v_;
};

template <typename C>
concept ConstantType = requires(C const& c) {
  { IcConstantIndex(c) } -> std::convertible_to<size_t>;
  typename C::constant_insertion_type;
  {
    IcConstantAppend(std::declval<ConstantComponentAppender&>,
                     std::declval<typename C::constant_insertion_type const&>())
    } -> std::same_as<void>;
};

namespace internal_constant {

struct Index {
  uint32_t kind;
  uint32_t index;
};

struct HashEqBase {
  explicit constexpr HashEqBase(
      std::vector<uint64_t> const& components NTH_ATTRIBUTE(lifetimebound))
      : components_(components) {}

  std::span<uint64_t const> SpanFor(uint32_t index) const {
    uint64_t size = components_[index];
    return std::span<uint64_t const>(&components_[index + 1], size);
  }

  std::vector<uint64_t> const& components_;
};

template <ConstantType... Cs>
struct Hash : HashEqBase {
  explicit constexpr Hash(
      std::vector<uint64_t> const& components NTH_ATTRIBUTE(lifetimebound))
      : HashEqBase(components) {}
  size_t operator()(Index i) const { return Hashers[i.kind](SpanFor(i.index)); }

 private:

  using hasher_type = size_t (*)(std::span<uint64_t const>);
  static constexpr hasher_type Hashers[sizeof...(Cs)] = {
      +[](std::span<uint64_t const> components) -> size_t {
        return static_cast<size_t>(IcHashConstant(nth::type<Cs>, components));
      }...};
};

template <ConstantType... Cs>
struct Eq : HashEqBase {
  explicit constexpr Eq(
      std::vector<uint64_t> const& components NTH_ATTRIBUTE(lifetimebound))
      : HashEqBase(components) {}
  bool operator()(Index lhs, Index rhs) const { std::abort(); }
};

}  // namespace internal_constant

template <ConstantType... Cs>
struct ConstantTable {
  std::vector<uint64_t> components_;
  absl::flat_hash_set<internal_constant::Index, internal_constant::Hash<Cs...>,
                      internal_constant::Eq<Cs...>>
      index_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_CONSTANT_H
