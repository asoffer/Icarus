#include <string>
#include <vector>

#include "base/log.h"
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

#define CHECK(...) MATCH(::test::Checker{&stats_}, __VA_ARGS__)

struct Checker {
  Checker(test::Statistics* stats) : stats_(stats) {}
  template <typename L, typename R>
  void operator()(::matcher::ExprMatchResult<L, R> const& result) const {
    ++stats_->expectations;
    if (result.matched) {
      ++stats_->passes;
    } else {
      using base::stringify;
      base::Logger(base::LogFormatterWithoutFunction)
          << "\033[0;1;31mCheck failed\n"
             "    \033[0;1;37mExpected:\033[0m "
          << result.expr_string
          << "\n"
             "         \033[0;1;37mLHS:\033[0m "
          << stringify(result.lhs)
          << "\n"
             "         \033[0;1;37mRHS:\033[0m "
          << stringify(result.rhs) << "\n";
    }
  }

  template <typename T>
  void operator()(::matcher::MatchResult<T> const& match_result) {
    ++stats_->expectations;
    if (!match_result.description.has_value()) {
      ++stats_->passes;
    } else {
      using base::stringify;
      base::Logger(base::LogFormatterWithoutFunction)
          << "\033[0;1;31m Check failed\n"
             "  \033[0;1;37mExpression:\033[0m "
          << match_result.expr.string()
          << "\n"
             "    \033[0;1;37mExpected:\033[0m "
          << *match_result.description << "\n"
          << "      \033[0;1;37mActual:\033[0m "
          << stringify(match_result.expr.value()) << "\n";
    }
  }

  test::Statistics* stats_ = nullptr;
};

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
