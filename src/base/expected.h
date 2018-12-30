#include "base/debug.h"

#include <type_traits>
#include <string>
#include <string_view>

namespace base {
struct unexpected {
  explicit unexpected(char const *s) : reason_(s) {}
  explicit unexpected(std::string_view s) : reason_(s) {}
  explicit unexpected(std::string s) : reason_(std::move(s)) {}

  std::string to_string() const { return reason_; }
  friend std::ostream &operator<<(std::ostream &os, unexpected const &u) {
    return os << u.reason_;
  }

 private:
  std::string reason_;
};

template <typename T>
struct expected {
  static_assert(!std::is_same_v<T, unexpected>);

  expected(T &&val) : val_(std::forward<T>(val)) {}

  template <typename... Args>
  explicit expected(Args &&... args) : val_(T{std::forward<Args>(args)...}) {}
  expected(unexpected u) : val_(std::move(u)) {}

  T *operator->() { return std::get_if<T>(&val_); }
  T const *operator->() const { return std::get_if<T>(&val_); }

  T &operator*() & { return std::get<T>(val_); }
  T const &operator*() const & { return std::get<T>(val_); }
  T &&operator*() && { return std::get<T>(std::move(val_)); }

  bool has_value() const { return std::holds_alternative<T>(val_); }
  unexpected error() const { return std::get<unexpected>(val_); }

  explicit operator bool() const { return has_value(); }

 private:
  std::variant<T, unexpected> val_;
};
}  // namespace base
