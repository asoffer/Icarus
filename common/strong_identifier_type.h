#ifndef ICARUS_COMMON_STRONG_IDENTIFIER_TYPE_H
#define ICARUS_COMMON_STRONG_IDENTIFIER_TYPE_H

#include <type_traits>
#include <utility>

namespace ic {

template <typename T, typename RepType>
struct StrongIdentifierType {
  using underlying_type = RepType;

  explicit constexpr StrongIdentifierType() requires(
      std::is_constructible_v<underlying_type>) = default;

  explicit constexpr StrongIdentifierType(underlying_type const &value)
      : value_(value) {}
  explicit constexpr StrongIdentifierType(underlying_type &&value)
      : value_(std::move(value)) {}

  friend bool operator==(StrongIdentifierType, StrongIdentifierType) = default;

  template <typename H>
  friend H AbslHashValue(H h, StrongIdentifierType n) {
    return H::combine(std::move(h), n.value_);
  }

  underlying_type const &value() const & { return value_; }
  underlying_type &&value() && { return std::move(value_); }

 private:
  underlying_type value_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_STRONG_IDENTIFIER_TYPE_H
