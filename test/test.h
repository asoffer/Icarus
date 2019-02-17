#include <iostream>
#include <string>
#include <vector>

#include "base/matchers.h"
#include "base/stringify.h"

namespace test {
struct Test;
static std::vector<Test*> AllTests{};
struct Statistics {
  int expectations = 0;
  int passes       = 0;
};

struct Test {
  Test() { AllTests.push_back(this); }
  Statistics Run() {
    stats_ = {};
    this->Body();
    return stats_;
  }
  virtual void Body() = 0;

  Statistics stats_;
};

#define TEST(name)                                                             \
  struct TEST_##name : public ::test::Test {                                   \
    static TEST_##name Instance;                                               \
    void Body() override;                                                      \
  };                                                                           \
                                                                               \
  TEST_##name TEST_##name::Instance;                                           \
  void TEST_##name::Body()

#define CHECK(...) MATCH(CHECK_MATCH, CHECK_EXPR, __VA_ARGS__)

#define CHECK_EXPR(expr)                                                       \
  [&](auto const& result) {                                                    \
    ++stats_.expectations;                                                     \
    if (result.matched) {                                                      \
      ++stats_.passes;                                                         \
    } else {                                                                   \
      using base::stringify;                                                   \
      /* TODO specify output logger? */                                        \
      std::cerr << "\n\033[0;1;34m[" __FILE__ ": " << std::to_string(__LINE__) \
                << "]\033[0;1;31m Check failed"                                \
                   "\n    \033[0;1;37mExpected:\033[0m " #expr                 \
                   "\n         \033[0;1;37mLHS:\033[0m "                       \
                << stringify(result.lhs)                                       \
                << "\n         \033[0;1;37mRHS:\033[0m "                       \
                << stringify(result.rhs) << "\n";                              \
    }                                                                          \
  }(MATCH_EXPR(expr))

#define CHECK_MATCH(expr, matcher)                                             \
  [&](auto const& e, auto const& m) {                                          \
    ++stats_.expectations;                                                     \
    using expr_type  = std::decay_t<decltype(e)>;                              \
    auto description = m.template With<expr_type>().match_and_describe(e);     \
    if (!description.has_value()) {                                            \
      ++stats_.passes;                                                         \
    } else {                                                                   \
      using base::stringify;                                                   \
      /* TODO specify output logger? */                                        \
      std::cerr << "\n\033[0;1;34m[" __FILE__ ": " << std::to_string(__LINE__) \
                << "]\033[0;1;31m Check failed\n"                              \
                   "  \033[0;1;37mExpression:\033[0m " #expr                   \
                   "\n"                                                        \
                   "    \033[0;1;37mExpected:\033[0m "                         \
                << *description << "\n"                                        \
                << "      \033[0;1;37mActual:\033[0m " << stringify(e)         \
                << "\n";                                                       \
    }                                                                          \
  }((expr), (matcher))

}  // namespace test

#ifndef ICARUS_CUSTOM_MAIN_TEST
int main() {
  bool found_error = false;
  for (test::Test* test : test::AllTests) {
    auto stats = test->Run();
    if (stats.expectations != stats.passes) { found_error = true; }
  }
  return found_error ? 1 : 0;
}
#endif
