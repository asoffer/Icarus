#ifndef ICARUS_BASE_COMPILE_TIME_STRING_H
#define ICARUS_BASE_COMPILE_TIME_STRING_H

#include <cstring>
#include <string_view>

namespace base {

// A string-type that can be used directly as a template parameter.
template <size_t N>
struct CompileTimeString {
  // An implicit constructor, along with the deduction guide allows strings to
  // be bound to template parameters.
  constexpr CompileTimeString(char const (&s)[N + 1]) {
    for (size_t i = 0; i < N; ++i) { data[i] = s[i]; }
  }

  constexpr bool operator==(CompileTimeString const &) const = default;
  constexpr bool operator!=(CompileTimeString const &) const = default;

  friend constexpr bool operator==(std::string_view s,
                                   CompileTimeString const &f) {
    return s.size() == N and std::memcmp(s.data(), f.data.data(), N) == 0;
  }
  friend constexpr bool operator==(CompileTimeString const &f,
                                   std::string_view s) {
    return s.size() == N and std::memcmp(s.data(), f.data.data(), N) == 0;
  }
  friend constexpr bool operator!=(std::string_view s,
                                   CompileTimeString const &f) {
    return not(s == f);
  }
  friend constexpr bool operator!=(CompileTimeString const &f,
                                   std::string_view s) {
    return not(s == f);
  }

  std::array<char, N> data;
};

template <size_t N>
CompileTimeString(char const (&s)[N]) -> CompileTimeString<N - 1>;

}  // namespace base

#endif  // ICARUS_BASE_COMPILE_TIME_STRING_H
