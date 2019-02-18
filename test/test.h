#include <string>
#include <vector>

#include "base/log.h"
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
  bool operator()(::matcher::ExprMatchResult<L, R> const& result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) const {
    ++stats_->expectations;
    if (result.matched) {
      ++stats_->passes;
    } else {
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31mCheck failed\n"
             "    \033[0;1;37mExpected:\033[0m "
          << result.expr_string
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
    ++stats_->expectations;
    if (!match_result.description.has_value()) {
      ++stats_->passes;
    } else {
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31m Check failed\n"
             "  \033[0;1;37mExpression:\033[0m "
          << match_result.expr.string()
          << "\n"
             "    \033[0;1;37mExpected:\033[0m "
          << *match_result.description << "\n"
          << "      \033[0;1;37mActual:\033[0m "
          << StringifyAndEscape(match_result.expr.value()) << "\n";
    }
    return match_result.description.has_value();
  }

 private:
  test::Statistics* stats_ = nullptr;
};

#define REQUIRE(...)                                                           \
  do {                                                                         \
    if (!(MATCH(::test::Checker{&stats_}, __VA_ARGS__))) { return; }           \
  } while (false)

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
