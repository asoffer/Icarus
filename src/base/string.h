#ifndef ICARUS_BASE_STRING_H
#define ICARUS_BASE_STRING_H

#include <memory>
#include <sstream>
#include <string>
#include <variant>

namespace base::internal {
template <int N>
struct dispatch_rank_count_down : public dispatch_rank_count_down<N - 1> {};
template <>
struct dispatch_rank_count_down<0> {};

constexpr int MaxDispatch = 6;
template <int N>
using dispatch_rank = dispatch_rank_count_down<MaxDispatch - N>;

template <typename T>
std::string stringify(T &&t);

#define DEFINE_RANKED_STRINGIFY(rank, type)                                    \
  template <typename T>                                                        \
  auto stringify(dispatch_rank<rank>, T &&val)                                 \
      ->decltype(std::enable_if_t<std::is_same_v<std::decay_t<T>, type>>(),       \
                 std::string())
DEFINE_RANKED_STRINGIFY(0, bool) { return val ? "true" : "false"; }
DEFINE_RANKED_STRINGIFY(0, const char *) { return val; }
DEFINE_RANKED_STRINGIFY(0, std::string) { return val; }
DEFINE_RANKED_STRINGIFY(0, char) { return std::string(1, val); }
DEFINE_RANKED_STRINGIFY(0, int) { return std::to_string(val); }
DEFINE_RANKED_STRINGIFY(0, size_t) { return std::to_string(val); }
#undef DEFINE_RANKED_STRINGIFY

inline auto stringify(dispatch_rank<0>, std::nullptr_t) -> std::string {
  return "nullptr";
}

template <typename Container>
auto stringify(dispatch_rank<1>, const Container &t)
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
auto stringify(dispatch_rank<1>, T *ptr)
    -> decltype(stringify(dispatch_rank<0>{}, *std::declval<T>()),
                std::string()) {
  return ptr == nullptr ? "null" : "ptr(" + stringify(*ptr) + ")";
}

template <typename T>
auto stringify(dispatch_rank<2>, T *ptr) -> std::string {
  return std::to_string(reinterpret_cast<uintptr_t>(ptr));
}

template <typename T>
auto stringify(dispatch_rank<2>, T &&val)
    -> decltype(std::declval<std::stringstream>() << std::declval<T>(),
                std::string()) {
  std::stringstream ss;
  ss << std::forward<T>(val);
  return ss.str();
}

template <typename T>
auto stringify(dispatch_rank<2>, const std::unique_ptr<T> &ptr) -> std::string {
  return ptr == nullptr ? "unique(null)"
                        : "unique_ptr(" + stringify(*ptr) + ")";
}

template <typename T>
auto stringify(dispatch_rank<3>, T &&val)
    -> decltype(std::declval<T>().to_string(), std::string()) {
  return val.to_string();
}

template <typename... Args>
auto stringify(dispatch_rank<3>, const std::variant<Args...> &v)
  -> std::string {
  return std::visit([](auto &&v) { return stringify(v); }, v);
}

template <typename Pair>
auto stringify(dispatch_rank<3>, const Pair &p)
  -> decltype(std::enable_if_t<std::is_same_v<
                  Pair, std::pair<decltype(p.first), decltype(p.second)>>>(),
              std::string()) {
return "(" + stringify(p.first) + ", " + stringify(p.second) + ")";
}
template <typename T>
auto stringify(dispatch_rank<5>, T val)
    -> decltype(std::enable_if_t<std::is_enum_v<std::decay_t<T>>>(),
                std::string()) {
  return stringify(static_cast<std::underlying_type_t<T>>(val));
}

template <typename T>
auto stringify(dispatch_rank<6>, T &&val) -> std::string {
  return "<object>";
}

template <typename T>
std::string stringify(T &&t) {
  return internal::stringify(internal::dispatch_rank<0>{}, std::forward<T>(t));
}
}  // namespace base::internal

#endif // ICARUS_BASE_STRING_H
