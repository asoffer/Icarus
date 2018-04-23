#ifndef ICARUS_BASE_STRING_H
#define ICARUS_BASE_STRING_H

#include <variant>
#include <sstream>
#include <string>

namespace base::internal {
template <int N>
struct dispatch_rank : public dispatch_rank<N - 1> {};
template <>
struct dispatch_rank<0> {};

template <typename T>
std::string stringify(T &&t);

template <typename T>
auto stringify(dispatch_rank<8>, const T &s)
    -> decltype(std::enable_if_t<std::is_same_v<T, std::string>>(),
                std::string()) {
  return s;
}

template <typename T>
auto stringify(dispatch_rank<7>, nullptr_t) -> std::string {
  return "nullptr";
}

template <typename T>
auto stringify(dispatch_rank<7>, const T &s)
    -> decltype(std::enable_if_t<std::is_same_v<T, std::string_view>>(),
                std::string()) {
  return std::string(s);
}

template <typename Container>
auto stringify(dispatch_rank<7>, const Container &t)
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

template <typename Pair>
auto stringify(dispatch_rank<7>, const Pair &p)
    -> decltype(std::enable_if_t<std::is_same_v<
                    Pair, std::pair<decltype(p.first), decltype(p.second)>>>(),
                std::string()) {
  return "(" + stringify(p.first) + ", " + stringify(p.second) + ")";
}

template <typename T>
auto stringify(dispatch_rank<6>, const T &t)
    -> decltype((std::string)std::declval<T>(), std::string()) {
  return t;
}

template <typename... Args>
auto stringify(dispatch_rank<5>, const std::variant<Args...> &v)
    -> std::string {
  return std::visit([](auto &&v) { return stringify(v); }, v);
}

template <typename T>
auto stringify(dispatch_rank<5>, const T &t)
    -> decltype(std::declval<T>().to_string(), std::string()) {
  return t.to_string();
}

template <typename T>
auto stringify(dispatch_rank<4>, const T &t)
    -> decltype(std::declval<T>() == nullptr, std::declval<T>()->to_string(),
                std::string()) {
  return t == nullptr ? "0x0" : t->to_string();
}

template <typename T>
auto stringify(dispatch_rank<3>, T c)
    -> decltype(std::enable_if_t<std::is_same_v<T, char>>(), std::string()) {
  if (c == '\0') { return "\\0"; }
  return std::string(1, c);
}

template <typename T>
auto stringify(dispatch_rank<3>, T b)
    -> decltype(std::enable_if_t<std::is_same_v<T, bool>>(), std::string()) {
  return b ? "true" : "false";
}

template <typename T>
auto stringify(dispatch_rank<2>, T *ptr) -> std::string {
  std::stringstream ss;
  ss << ptr;
  return ss.str();
}

template <typename T>
auto stringify(dispatch_rank<1>, T &&t)
    -> decltype(std::to_string(std::declval<T>()), std::string()) {
  return std::to_string(std::forward<T>(t));
}

template <typename T>
auto stringify(dispatch_rank<0>, const T &) -> std::string {
  return "<object>";
}

template <typename T>
std::string stringify(T &&t) {
  return internal::stringify(internal::dispatch_rank<8>{}, std::forward<T>(t));
}
}  // namespace base::internal

#endif // ICARUS_BASE_STRING_H
