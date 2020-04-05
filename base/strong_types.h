#ifndef ICARUS_BASE_STRONG_TYPES_H
#define ICARUS_BASE_STRONG_TYPES_H

#include <string>
#include <type_traits>
#include <utility>

#include "absl/strings/str_cat.h"
#include "base/meta.h"
#include "base/stringify.h"

namespace base {

struct EnableEqualityComparisons {};
struct EnableComparisons : EnableEqualityComparisons {};
struct EnableArithmetic {};
struct EnableRawArithmetic : EnableArithmetic {};
struct EnableHashing : EnableEqualityComparisons {};

namespace internal {

template <typename... Tags>
struct StrongTypeCrtp : public Tags... {};

template <typename Tag, typename UnderlyingType, typename CrtpTags>
struct Strong {
  using tag_type        = Tag;
  using underlying_type = UnderlyingType;

  constexpr Strong() = default;
  constexpr explicit Strong(UnderlyingType num) : value(num) {}
  std::string to_string() const {
    using base::stringify;
    return stringify(value);
  }

  UnderlyingType value{};
};

template <typename Tag, typename UnderlyingType, typename CrtpTags, typename H,
          typename std::enable_if_t<
              std::is_base_of_v<EnableHashing, CrtpTags>>* = nullptr>
H AbslHashValue(H h, Strong<Tag, UnderlyingType, CrtpTags> v) {
  return H::combine(std::move(h), v.value);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator++(
    Strong<Tag, UnderlyingType, CrtpTags>& x) {
  ++x.value;
  return x;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator++(Strong<Tag, UnderlyingType, CrtpTags>& x, int) {
  Tag prev(x.value);
  ++x;
  return prev;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator--(
    Strong<Tag, UnderlyingType, CrtpTags>& x) {
  --x.value;
  return x;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator--(Strong<Tag, UnderlyingType, CrtpTags>& x, int) {
  Tag prev(x.value);
  --x;
  return prev;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator+=(
    Strong<Tag, UnderlyingType, CrtpTags>& lhs,
    base::DeductionBlockerT<UnderlyingType> n) {
  lhs.value += n;
  return lhs;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator+=(
    Strong<Tag, UnderlyingType, CrtpTags>& lhs,
    Strong<Tag, UnderlyingType, CrtpTags>& rhs) {
  lhs.value += rhs.value;
  return lhs;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator-=(
    Strong<Tag, UnderlyingType, CrtpTags>& lhs,
    base::DeductionBlockerT<UnderlyingType> n) {
  lhs.value -= n;
  return lhs;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableArithmetic, CrtpTags>>* = nullptr>
constexpr Strong<Tag, UnderlyingType, CrtpTags>& operator-=(
    Strong<Tag, UnderlyingType, CrtpTags>& lhs,
    Strong<Tag, UnderlyingType, CrtpTags>& rhs) {
  lhs.value -= rhs.value;
  return lhs;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator+(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                        base::DeductionBlockerT<UnderlyingType> rhs) {
  return Tag(lhs.value + rhs);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator+(base::DeductionBlockerT<UnderlyingType> lhs,
                        Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return Tag(lhs + rhs.value);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator+(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                        Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return Tag(lhs.value + rhs.value);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator-(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                        base::DeductionBlockerT<UnderlyingType> rhs) {
  return Tag(lhs.value - rhs);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableRawArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator-(base::DeductionBlockerT<UnderlyingType> lhs,
                        Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return Tag(lhs - rhs.value);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableArithmetic, CrtpTags>>* = nullptr>
constexpr Tag operator-(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                        Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return Tag(lhs.value - rhs.value);
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<std::is_base_of_v<EnableEqualityComparisons,
                                                      CrtpTags>>* = nullptr>
constexpr bool operator==(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                          Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return lhs.value == rhs.value;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableComparisons, CrtpTags>>* = nullptr>
constexpr bool operator<(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                         Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return lhs.value < rhs.value;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableComparisons, CrtpTags>>* = nullptr>
constexpr bool operator>(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                         Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return lhs.value > rhs.value;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableComparisons, CrtpTags>>* = nullptr>
constexpr bool operator<=(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                          Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return lhs.value <= rhs.value;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<
              std::is_base_of_v<EnableComparisons, CrtpTags>>* = nullptr>
constexpr bool operator>=(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                          Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return lhs.value >= rhs.value;
}

template <typename Tag, typename UnderlyingType, typename CrtpTags,
          typename std::enable_if_t<std::is_base_of_v<EnableEqualityComparisons,
                                                      CrtpTags>>* = nullptr>
constexpr bool operator!=(Strong<Tag, UnderlyingType, CrtpTags> lhs,
                          Strong<Tag, UnderlyingType, CrtpTags> rhs) {
  return not(lhs == rhs);
}

}  // namespace internal
}  // namespace base

#define ICARUS_BASE_DEFINE_STRONG_TYPE(name, default_value, ...)               \
  struct name : public base::internal::Strong<                                 \
                    name, std::decay_t<decltype(default_value)>,               \
                    base::internal::StrongTypeCrtp<__VA_ARGS__>> {             \
    name()                                                                     \
        : base::internal::Strong<name, underlying_type,                        \
                                 base::internal::StrongTypeCrtp<__VA_ARGS__>>( \
              default_value) {}                                                \
                                                                               \
    template <typename T,                                                      \
              std::enable_if_t<std::is_convertible_v<T, underlying_type> and   \
                                   std::is_arithmetic_v<T>,                    \
                               int> = 0>                                       \
    explicit constexpr name(T val)                                             \
        : base::internal::Strong<name, underlying_type,                        \
                                 base::internal::StrongTypeCrtp<__VA_ARGS__>>( \
              val) {}                                                          \
                                                                               \
    template <typename T,                                                      \
              std::enable_if_t<std::is_convertible_v<T, underlying_type> and   \
                                   not std::is_arithmetic_v<T>,                \
                               int> = 0>                                       \
    explicit name(T val)                                                       \
        : base::internal::Strong<name, underlying_type,                        \
                                 base::internal::StrongTypeCrtp<__VA_ARGS__>>( \
              val) {}                                                          \
                                                                               \
    name(name&&) noexcept = default;                                           \
    name(name const&)     = default;                                           \
    name& operator=(name&&) noexcept = default;                                \
    name& operator=(name const&) = default;                                    \
                                                                               \
    friend std::ostream& operator<<(std::ostream& os, name val) {              \
      return os << absl::StrCat(#name, "(", val.value, ")");                   \
    }                                                                          \
                                                                               \
    friend std::string stringify(name val) {                                   \
      return absl::StrCat(#name, "(", val.value, ")");                         \
    }                                                                          \
  };                                                                           \
                                                                               \
  static_assert(true)

#endif  // ICARUS_BASE_STRONG_TYPES_H
