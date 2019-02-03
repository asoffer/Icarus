#ifndef ICARUS_TEST_H
#define ICARUS_TEST_H

#include <functional>
#include <iostream>
#include <type_traits>
#include <variant>
#include "base/check.h"

namespace test {
template <typename T>
auto Holds() {
  return [](const auto& v) -> std::string {
    return std::holds_alternative<T>(v) ? "" : "Holds unexpected alternative.";
  };
}

template <typename T>
auto Holds(T&& val) {
  return [val{std::forward<T>(val)}](const auto& v)->std::string {
    if (auto* vptr = std::get_if<std::decay_t<T>>(&v)) {
      if (*vptr == val) {
        return "";
      } else {
        return "A variant holding " + base::internal::stringify(val);
      }
    } else {
      return "Holds unexpected alternative.";
    }
  };
}
}  // namespace test

#define EXPECT(...)                                                            \
  do {                                                                         \
    if (::base::check::internal::LhsStealer(__FILE__, __LINE__, #__VA_ARGS__)  \
        << __VA_ARGS__) {                                                      \
      ++this->pass_count;                                                      \
    }                                                                          \
    ++this->total_count;                                                       \
  } while (false)

#define TEST(name)                                                             \
  namespace test {                                                             \
  struct TEST_##name {                                                         \
    static bool test_runner;                                                   \
    int pass_count  = 0;                                                       \
    int total_count = 0;                                                       \
    void operator()();                                                         \
  };                                                                           \
  bool TEST_##name::test_runner = []() {                                       \
    std::cerr << "-- TESTING " #name " "                                       \
              << std::string(81 - sizeof("-- TESTING " #name " "), '-')        \
              << "\n";                                                         \
    TEST_##name test{};                                                        \
    test();                                                                    \
    std::cerr << "TEST RESULTS FOR " << #name << ": " << test.pass_count       \
              << " / " << test.total_count << " expectations passed.\n"        \
              << std::string(80, '-') << "\n\n";                               \
    return true;                                                               \
  }();                                                                         \
  }                                                                            \
  void test::TEST_##name::operator()()

int main() {}
#endif  // ICARUS_TEST_H
