#ifndef ICARUS_BASE_STRING_H
#define ICARUS_BASE_STRING_H

#include <iomanip>
#include <optional>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <variant>

#include "base/tuple.h"

namespace base {
template <typename T>
std::string stringify(T const &t);

namespace internal {

// Dispatch ranks increase in terms of priority. Highest rank wins.
template <int N>
struct dispatch_rank : public dispatch_rank<N - 1> {};
template <>
struct dispatch_rank<0> {};

template <typename T>
auto stringify_dispatch(dispatch_rank<0>, T const &val) -> std::string {
  return std::string{"<object with typeid "} + typeid(T).name() + ">";
}

#define DEFINE_RANKED_STRINGIFY(rank, ...)                                     \
  template <typename T>                                                        \
  auto stringify_dispatch(dispatch_rank<rank>, T value)                        \
      ->decltype(                                                              \
          std::enable_if_t<                                                    \
              std::is_same_v<std::decay_t<T>, std::decay_t<__VA_ARGS__>>>(),   \
          std::string{})

DEFINE_RANKED_STRINGIFY(3, std::nullptr_t) { return "nullptr"; }
DEFINE_RANKED_STRINGIFY(2, bool) { return value ? "true" : "false"; }
DEFINE_RANKED_STRINGIFY(3, char const *) {
  return (value == nullptr) ? "null char const*" : value;
}
DEFINE_RANKED_STRINGIFY(3, std::string const&) { return value; }
DEFINE_RANKED_STRINGIFY(3, std::string_view) { return std::string{value}; }
DEFINE_RANKED_STRINGIFY(2, char) {
  char const c[2] = {value, '\0'};
  return std::string(c);
}
DEFINE_RANKED_STRINGIFY(2, short) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, short unsigned) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, int) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, int unsigned) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, long) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, long unsigned) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, long long) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, long long unsigned) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, float) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, double) { return std::to_string(value); }
DEFINE_RANKED_STRINGIFY(2, long double) { return std::to_string(value); }
#undef DEFINE_RANKED_STRINGIFY

// Should be lower rank than const char *.
template <typename T>
std::string stringify_dispatch(dispatch_rank<2>, T *ptr) {
  return stringify_dispatch(dispatch_rank<2>{}, static_cast<T const *>(ptr));
}

template <typename T>
std::string stringify_dispatch(dispatch_rank<2>, T const *ptr) {
  std::stringstream ss;
  ss << "0x" << std::setw(2 * sizeof(uintptr_t)) << std::setfill('0')
     << std::hex << reinterpret_cast<uintptr_t>(ptr);
  return ss.str();
}

template <typename A, typename B>
std::string stringify_dispatch(dispatch_rank<2>, std::pair<A, B> const &pair) {
  return "(" + stringify(pair.first) + ", " + stringify(pair.second) + ")";
}

template <typename... Ts>
std::string stringify_dispatch(dispatch_rank<2>, std::tuple<Ts...> const &tup) {
  if constexpr (sizeof...(Ts) == 0) {
    return "()";
  } else if constexpr (sizeof...(Ts) == 1) {
    return "(" + stringify(std::get<0>(tup)) + ")";
  } else {
    std::array<std::string, sizeof...(Ts)> results;
    size_t index = 0;
    tuple::for_each([&](auto const &arg) { results[index++] = stringify(arg); },
                    tup);

    std::string result = "(" + results[0];
    for (size_t i = 1; i < sizeof...(Ts); ++i) { result += ", " + results[i]; }
    return result + ")";
  }
}

template <typename... Ts>
std::string stringify_dispatch(dispatch_rank<2>,
                               std::variant<Ts...> const &var) {
  return std::visit([](auto const &v) { return stringify(v); }, var);
}

template <typename T>
std::string stringify_dispatch(dispatch_rank<2>, std::optional<T> const &opt) {
  return opt.has_value() ? stringify(*opt) : "nullopt";
}

template <typename T>
auto stringify_dispatch(dispatch_rank<1>, T const &t)
    -> decltype(std::declval<std::stringstream>() << t, std::string()) {
  std::stringstream ss;
  ss << t;
  return ss.str();
}

template <typename Container>
auto stringify_dispatch(dispatch_rank<2>, Container const &t)
    -> decltype(std::declval<Container>().begin(),
                std::declval<Container>().end(),
                ++std::declval<Container>().begin(),
                *std::declval<Container>().begin(), std::string()) {
  std::stringstream ss;
  if (t.empty()) {
    ss << "[]";
  } else {
    auto iter = t.begin();
    ss << '[' << stringify(*iter);
    ++iter;
    while (iter != t.end()) {
      ss << ", " << stringify(*iter);
      ++iter;
    }
    ss << ']';
  }
  return ss.str();
}

template <typename T>
auto stringify_dispatch(dispatch_rank<3>, T const &val)
    -> decltype(std::declval<T>().to_string(), std::string()) {
  return val.to_string();
}

}  // namespace internal

template <typename T>
std::string stringify(T const &t) {
  return internal::stringify_dispatch(internal::dispatch_rank<3>{}, t);
}
}  // namespace base

#endif  // ICARUS_BASE_STRING_H
