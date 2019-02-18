#include <string>
#include <vector>

#include "base/log.h"
#include "base/macros.h"
#include "base/matchers.h"
#include "base/stringify.h"
#include "init/signal.h"

namespace test {
namespace internal {
char const* EscapeChar(char c) {
  switch (c) {
    case '\0': return "\\0";
    case '\n': return "\\n";
    case '\t': return "\\t";
    case '"': return "\\\"";
    default: return nullptr;
  }
}

template <typename T>
std::string StringifyAndEscape(T const& t) {
  using type = std::decay_t<T>;
  if constexpr (std::is_same_v<type, std::string> ||
                std::is_same_v<type, std::string_view>) {
    std::string result = "\"";
    // TODO clean up performance here.
    for (char c : t) {
      auto escaped = EscapeChar(c);
      if (escaped) { result += escaped; }
    }
    return result + "\"";
  } else if constexpr (std::is_same_v<type, char*> ||
                       std::is_same_v<type, char const*>) {
    char const * ptr = t;
    std::string result = "\"";
    // TODO clean up performance here.
    while (*ptr != '\0') {
      auto escaped = EscapeChar(*ptr);
      if (escaped) { result += escaped; }
      ++ptr;
    }
    return result + "\"";
  } else {
    using base::stringify;
    return stringify(t);
  }
}
}  // namespace internal

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
  virtual char const* test_name() const = 0;

  Statistics stats_;
};

#define TEST(name)                                                             \
  struct TEST_##name : public ::test::Test {                                   \
    static TEST_##name Instance;                                               \
    char const* test_name() const override { return #name; }                   \
    void Body() override;                                                      \
  };                                                                           \
                                                                               \
  TEST_##name TEST_##name::Instance;                                           \
  void TEST_##name::Body()

struct Checker {
  Checker(test::Test* test, char const* expr_str = nullptr)
      : test_(test), expr_str_(expr_str) {}
  template <typename L, typename R>
  bool operator()(::matcher::ExprMatchResult<L, R> const& result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) const {
    ++test_->stats_.expectations;
    if (result.matched) {
      ++test_->stats_.passes;
    } else {
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31mCheck failed in \033[0;1;37m[" << test_->test_name()
          << "]\033[0m\n"
             "    \033[0;1;37mExpected:\033[0m "
          << (expr_str_ ?expr_str_: result.expr_string)
          << "\n"
             "         \033[0;1;37mLHS:\033[0m "
          << internal::StringifyAndEscape(result.lhs)
          << "\n"
             "         \033[0;1;37mRHS:\033[0m "
          << internal::StringifyAndEscape(result.rhs) << "\n";
    }
    return result.matched;
  }

  template <typename T>
  bool operator()(::matcher::MatchResult<T> const& match_result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) {
    ++test_->stats_.expectations;
    if (!match_result.description.has_value()) {
      ++test_->stats_.passes;
    } else {
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31mCheck failed in \033[0;1;37m[" << test_->test_name()
          << "]\033[0m\n"
             "  \033[0;1;37mExpression:\033[0m "
          << (expr_str_ ? expr_str_ : match_result.expr.string())
          << "\n"
             "    \033[0;1;37mExpected:\033[0m "
          << *match_result.description << "\n"
          << "      \033[0;1;37mActual:\033[0m "
          << internal::StringifyAndEscape(match_result.expr.value()) << "\n";
    }
    return !match_result.description.has_value();
  }

 private:
  test::Test* test_ = nullptr;
  // If not null, an override for the expression string provided through MATCH.
  char const* expr_str_ = nullptr;
};

#define CHECK(...) MATCH(::test::Checker{this}, __VA_ARGS__)

#define REQUIRE(...)                                                           \
  do {                                                                         \
    if (!(MATCH(::test::Checker{this}, __VA_ARGS__))) { return; }              \
  } while (false)

#define REQUIRE_ASSIGN(var, expr)                                              \
  INTERNAL_TEST_REQUIRE_ASSIGN_(var, expr, CAT(expr__, __LINE__, __))
#define INTERNAL_TEST_REQUIRE_ASSIGN_(var, expr, tmp)                          \
  INTERNAL_TEST_REQUIRE_ASSIGN__(var, expr, tmp)

#define INTERNAL_TEST_REQUIRE_ASSIGN__(var, expr, temp)                        \
  auto&& temp = (expr);                                                        \
  using ::matcher::CastsTo;                                                    \
  do {                                                                         \
    if (!(MATCH(::test::Checker(this, #expr), temp, CastsTo<true>()))) {       \
      return;                                                                  \
    }                                                                          \
  } while (false);                                                             \
  var = *std::move(temp)

}  // namespace test

#ifndef ICARUS_CUSTOM_MAIN_TEST
int main() {
  init::InstallSignalHandlers();
  bool found_error = false;
  for (test::Test* test : test::AllTests) {
    auto stats = test->Run();
    if (stats.expectations != stats.passes) { found_error = true; }
  }
  return found_error ? 1 : 0;
}
#endif
