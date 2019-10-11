#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <memory>

namespace base {
template <typename Fn>
struct defer {
  defer(Fn &&fn) : fn_(std::move(fn)) {}
  ~defer() { fn_(); }

 private:
  Fn fn_;
};
template <typename Fn>
defer(Fn &&)->defer<Fn>;

template <typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
overloaded(Ts...)->overloaded<Ts...>;

template <typename T>
constexpr bool always_false() {
  return false;
}

constexpr size_t Log2(size_t n) { return n == 1 ? 0 : 1 + Log2(n / 2); }
}  // namespace base
#endif  // ICARUS_BASE_UTIL_H
