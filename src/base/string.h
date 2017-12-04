#ifndef ICARUS_BASE_STRING_H
#define ICARUS_BASE_STRING_H

#include <string>

namespace base {
template <typename... Args> using void_t = void;
namespace internal {
template <typename T, typename = void> struct stringifier {};

template <int N> struct dispatch_rank : public dispatch_rank<N - 1> {};
template <> struct dispatch_rank<0> {};

template <typename T>
auto stringify(dispatch_rank<4>, T &&t)
    -> decltype((std::string)std::declval<T>(), std::string()) {
  return t;
}

template <typename T>
auto stringify(dispatch_rank<3>, T &&t)
    -> decltype(std::declval<T>().to_string(), std::string()) {
  return t.to_string();
}

template <typename T>
auto stringify(dispatch_rank<2>, T &&t)
    -> decltype(std::declval<T>() == nullptr, std::declval<T>()->to_string(),
                std::string()) {
  return t == nullptr ? "0x0" : t->to_string();
}

template <typename T>
auto stringify(dispatch_rank<1>, T c)
    -> decltype(std::enable_if_t<std::is_same<T, char>::value>(),
                std::string()) {
  if (c == '\0') { return "\\0"; }
  return std::string(1, c);
}

template <typename T>
auto stringify(dispatch_rank<1>, T b)
    -> decltype(std::enable_if_t<std::is_same<T, bool>::value>(),
                std::string()) {
  return b ? "true" : "false";
}

template <typename T>
auto stringify(dispatch_rank<0>, T &&t)
    -> decltype(std::to_string(std::declval<T>()), std::string()) {
  return std::to_string(std::forward<T>(t));
}

template <typename T> std::string stringify(T &&t) {
  return internal::stringify(internal::dispatch_rank<4>{}, std::forward<T>(t));
}
} // namespace internal
} // namespace base

#endif // ICARUS_BASE_STRING_H
