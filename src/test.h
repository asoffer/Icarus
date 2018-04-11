#ifndef ICARUS_TEST_H
#define ICARUS_TEST_H

#include <iostream>
#include <type_traits>
#include "base/string.h"

namespace test::internal {
struct LhsStealer {
  using line_t = decltype(__LINE__);
  LhsStealer(const char* f, line_t l, const char* e)
      : file(f), line(l), expr(e) {}
  LhsStealer(const LhsStealer&) = default;
  const char *file;
  line_t line;
  const char *expr;
};

template <typename T>
struct Expr {
  Expr(LhsStealer s, T&& v) : stealer(s), val(std::forward<T>(v)) {}
  LhsStealer stealer;
  T val;
};

template <>
struct Expr<bool> {
  Expr(LhsStealer s, bool b) : stealer(s), val(b) {}

  LhsStealer stealer;
  bool val;
  operator bool() {
    if (val) { return true; }
    std::cerr << "Expectation failed (" << stealer.file << ", line #"
              << stealer.line << ")\n"
              << stealer.expr
              << "\n(Value was false but expected to be true).\n\n";
    return false;
  }
};

template <typename T>
Expr<T> operator<<(LhsStealer stealer, T &&val) {
  return Expr<T>(stealer, std::forward<T>(val));
}

template <typename T, typename U>
auto operator<<(const Expr<T>& lhs, U&&rhs) {
  return Expr<decltype(std::declval<T>() << std::declval<U>())>(
      lhs.stealer, lhs.val << std::forward<U>(rhs));
}

template <typename T, typename U>
auto operator>>(const Expr<T>& lhs, U&&rhs) {
  return Expr<decltype(std::declval<T>() >> std::declval<U>())>(
      lhs.stealer, lhs.val >> std::forward<U>(rhs));
}

#define MAKE_OPERATOR(op)                                                      \
  template <typename T, typename U>                                            \
  inline bool operator op(const Expr<T>& lhs, U&& rhs) {                       \
    if (lhs.val op rhs) { return true; }                                       \
    std::cerr << "Expectation failed (" << lhs.stealer.file << ", line #"      \
              << lhs.stealer.line << ")\n\n  " << lhs.stealer.expr             \
              << "\n\nleft-hand side:  " << base::internal::stringify(lhs.val) \
              << "\nright-hand side: " << base::internal::stringify(rhs)       \
              << "\n\n";                                                       \
    return false;                                                              \
  }

MAKE_OPERATOR(<)
MAKE_OPERATOR(<=)
MAKE_OPERATOR(==)
MAKE_OPERATOR(!=)
MAKE_OPERATOR(>=)
MAKE_OPERATOR(>)
}  // namespace test::internal

#define EXPECT(...)                                                            \
  do {                                                                         \
    if (::test::internal::LhsStealer(__FILE__, __LINE__, #__VA_ARGS__)         \
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
