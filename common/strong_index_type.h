#ifndef ICARUS_COMMON_STRONG_INDEX_TYPE_H
#define ICARUS_COMMON_STRONG_INDEX_TYPE_H

#include <limits>
#include <type_traits>
#include <utility>

namespace ic {

template <typename T, typename RepType, typename DiffType>
struct StrongIndexType {
  using underlying_type = RepType;
  using difference_type = DiffType;

  explicit constexpr StrongIndexType() requires(
      std::is_constructible_v<underlying_type>) = default;

  explicit constexpr StrongIndexType(underlying_type const &value)
      : value_(value) {}
  explicit constexpr StrongIndexType(underlying_type &&value)
      : value_(std::move(value)) {}

  friend auto operator<=>(StrongIndexType, StrongIndexType) = default;

  template <typename H>
  friend H AbslHashValue(H h, StrongIndexType n) {
    return H::combine(std::move(h), n.value_);
  }

  underlying_type const &value() const & { return value_; }
  underlying_type &&value() && { return std::move(value_); }

  constexpr T &operator+=(difference_type const &rhs) {
    value_ += rhs;
    return static_cast<T &>(*this);
  }

  constexpr T &operator+=(difference_type &&rhs) {
    value_ += std::move(rhs);
    return static_cast<T &>(*this);
  }

  constexpr T &operator-=(difference_type const &rhs) {
    value_ -= rhs;
    return static_cast<T &>(*this);
  }

  constexpr T &operator-=(difference_type &&rhs) {
    value_ -= std::move(rhs);
    return static_cast<T &>(*this);
  }

  constexpr T &operator++() {
    ++value_;
    return static_cast<T &>(*this);
  }

  constexpr T operator++(int) {
    auto copy = *this;
    ++value_;
    return copy;
  }

  constexpr T &operator--() {
    --value_;
    return static_cast<T &>(*this);
  }

  constexpr T operator--(int) {
    auto copy = *this;
    --value_;
    return copy;
  }

  friend constexpr T operator+(T const &lhs, difference_type const &rhs) {
    return T(lhs.value_ + rhs);
  }

  friend constexpr T operator+(T const &lhs, difference_type &&rhs) {
    return T(lhs.value_ + std::move(rhs));
  }

  friend constexpr T operator+(T &&lhs, difference_type const &rhs) {
    return T(std::move(lhs).value_ + rhs);
  }

  friend constexpr T operator+(T &&lhs, difference_type &&rhs) {
    return T(std::move(lhs).value_ + std::move(rhs));
  }

  friend constexpr T operator-(T const &lhs, difference_type const &rhs) {
    return T(lhs.value_ - rhs);
  }

  friend constexpr T operator-(T const &lhs, difference_type &&rhs) {
    return T(lhs.value_ - std::move(rhs));
  }

  friend constexpr T operator-(T &&lhs, difference_type const &rhs) {
    return T(std::move(lhs).value_ - rhs);
  }

  friend constexpr T operator-(T &&lhs, difference_type &&rhs) {
    return T(std::move(lhs).value_ - std::move(rhs));
  }

  friend constexpr T operator-(T const &lhs, T const &rhs) {
    return T(lhs.value_ - rhs.value_);
  }

  friend constexpr T operator-(T const &lhs, T &&rhs) {
    return T(lhs.value_ - std::move(rhs).value_);
  }

  friend constexpr T operator-(T &&lhs, T const &rhs) {
    return T(std::move(lhs).value_ - rhs.value_);
  }

  friend constexpr T operator-(T &&lhs, T &&rhs) {
    return T(std::move(lhs).value_ - std::move(rhs).value_);
  }

 private:
  underlying_type value_;
};

}  // namespace ic

namespace std {

template <typename T>
requires(std::is_base_of_v<
         ::ic::StrongIndexType<std::remove_cv_t<T>,
                               typename std::remove_cv_t<T>::underlying_type,
                               typename std::remove_cv_t<T>::difference_type>,
         std::remove_cv_t<T>>) struct numeric_limits<T>
    : private numeric_limits<typename std::remove_cv_t<T>::underlying_type> {
 private:
  using base_type =
      numeric_limits<typename std::remove_cv_t<T>::underlying_type>;

 public:
  using base_type::digits10;
  using base_type::has_denorm;
  using base_type::has_denorm_loss;
  using base_type::has_infinity;
  using base_type::has_quiet_NaN;
  using base_type::has_signaling_NaN;
  using base_type::is_bounded;
  using base_type::is_exact;
  using base_type::is_iec559;
  using base_type::is_integer;
  using base_type::is_signed;
  using base_type::is_specialized;
  using base_type::max_digits10;
  using base_type::max_exponent;
  using base_type::max_exponent10;
  using base_type::min_exponent;
  using base_type::min_exponent10;
  using base_type::radix;
  using base_type::round_style;
  using base_type::tinyness_before;
  using base_type::traps;
  static constexpr bool is_modulo = false;
  static constexpr std::remove_cv_t<T> min() {
    return std::remove_cv_t<T>{base_type::min()};
  }
  static constexpr std::remove_cv_t<T> max() {
    return std::remove_cv_t<T>{base_type::max()};
  }
  static constexpr std::remove_cv_t<T> lowest() {
    return std::remove_cv_t<T>{base_type::lowest()};
  }
  static constexpr std::remove_cv_t<T> epsilon() {
    return std::remove_cv_t<T>{base_type::epsilon()};
  }
  static constexpr std::remove_cv_t<T> round_error() {
    return std::remove_cv_t<T>{base_type::round_error()};
  }
  static constexpr std::remove_cv_t<T> infinity() {
    return std::remove_cv_t<T>{base_type::infinity()};
  }
  static constexpr std::remove_cv_t<T> quiet_NaN() {
    return std::remove_cv_t<T>{base_type::quiet_NaN()};
  }
  static constexpr std::remove_cv_t<T> signalling_NaN() {
    return std::remove_cv_t<T>{base_type::signalling_NaN()};
  }
  static constexpr std::remove_cv_t<T> denorm_min() {
    return std::remove_cv_t<T>{base_type::denorm_min()};
  }
};

}  // namespace std

#endif  // ICARUS_COMMON_STRONG_INDEX_TYPE_H
