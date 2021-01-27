#ifndef ICARUS_BASE_EXPECTED_H
#define ICARUS_BASE_EXPECTED_H

#include <string>
#include <string_view>
#include <type_traits>
#include <variant>

#include "absl/status/status.h"
#include "base/stringify.h"

namespace base {

template <typename T, typename E = absl::Status>
struct expected {
  static_assert(not std::is_same_v<T, E>);

  expected(T val) : val_(std::move(val)) {}

  template <typename... Args>
  explicit expected(Args &&...args) : val_(T{std::forward<Args>(args)...}) {}
  expected(E e) : val_(std::move(e)) {}

  T *operator->() { return std::get_if<T>(&val_); }
  T const *operator->() const { return std::get_if<T>(&val_); }

  T &operator*() & { return std::get<T>(val_); }
  T const &operator*() const & { return std::get<T>(val_); }
  T &&operator*() && { return std::get<T>(std::move(val_)); }

  bool has_value() const { return std::holds_alternative<T>(val_); }
  E error() const & { return std::get<E>(val_); }
  E &&error() && { return std::get<E>(std::move(val_)); }

  explicit operator bool() const { return has_value(); }

 private:
  std::variant<T, E> val_;
};

inline std::string stringify(absl::Status st) {
  return std::string(std::move(st).message());
}

template <typename T, typename E>
std::string stringify(expected<T, E> const &e) {
  if (e) {
    using base::stringify;
    return stringify(*e);
  }
  return stringify(e.error());
}

template <typename T, typename E>
std::ostream &operator<<(std::ostream &os, expected<T, E> const &e) {
  using base::stringify;
  return os << stringify(e);
}

}  // namespace base

#endif  // ICARUS_BASE_EXPECTED_H
