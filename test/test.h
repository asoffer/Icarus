#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "base/stringify.h"
#include "base/tuple.h"

#define EXPECT(...)                                                            \
  do {                                                                         \
    if (::base::check::internal::LhsStealer(__FILE__, __LINE__, #__VA_ARGS__)  \
        << __VA_ARGS__) {                                                      \
      ++this->pass_count;                                                      \
    }                                                                          \
    ++this->total_count;                                                       \
  } while (false)

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

#define CHECK(...)                                                             \
  CHECK_IMPL(CHECK_NUM_ARGS(__VA_ARGS__, CHECK_2_ARGS, CHECK_1_ARG),           \
             __VA_ARGS__)
#define CHECK_NUM_ARGS(x, y, macro, ...) macro
#define CHECK_IMPL(macro, ...) macro(__VA_ARGS__)

#define CHECK_1_ARG(x)
#define CHECK_2_ARGS(expr, matcher)                                            \
  do {                                                                         \
    ++stats_.expectations;                                                     \
    auto const& e                          = (expr);                           \
    std::optional<std::string> description = (matcher).match_and_describe(e);  \
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
  } while (false)

template <typename T>
struct Matcher {
  std::optional<std::string >match_and_describe(T const& input) {
    if (match(input)) { return std::nullopt; }
    return describe(true);
  }

  virtual std::string describe(bool positive) const = 0;
  virtual bool match(T const&) const                = 0;
};

template <typename T>
struct Not : public Matcher<T> {
  Not(Matcher<T> const& m) : m_(m) {}

  bool match(T const& input) const { return !m_.match(input); }
  std::string describe(bool positive) const override {
    return m_.describe(!positive);
  }

 private:
  Matcher<T> const& m_;
};

template <typename T>
inline Not<T> operator!(Matcher<T> const& m) {
  return Not{m};
}

template <typename T>
struct And : public Matcher<T> {
  And(Matcher<T> const& lhs, Matcher<T> const& rhs) : lhs_(lhs), rhs_(rhs) {}
  bool match(T const& input) const override {
    return lhs_.match(input) && rhs_.match(input);
  }
  std::string describe(bool positive) const override {
    return lhs_.describe(positive) + " and " + rhs_.describe(positive);
  }

 private:
  Matcher<T> const& lhs_;
  Matcher<T> const& rhs_;
};

template <typename T>
inline And<T> operator&&(Matcher<T> const& lhs, Matcher<T> const& rhs) {
  return And{lhs, rhs};
}

template <typename T>
struct Or : public Matcher<T> {
  Or(Matcher<T> const& lhs, Matcher<T> const& rhs) : lhs_(lhs), rhs_(rhs) {}
  bool match(T const& input) const override {
    return lhs_.match(input) || rhs_.match(input);
  }
  std::string describe(bool positive) const override {
    return lhs_.describe(positive) + " or " + rhs_.describe(positive);
  }

 private:
  Matcher<T> const& lhs_;
  Matcher<T> const& rhs_;
};

template <typename T>
inline Or<T> operator||(Matcher<T> const& lhs, Matcher<T> const& rhs) {
  return Or{lhs, rhs};
}

template <typename... Ts>
struct Tuple : public Matcher<std::tuple<Ts...>> {
  Tuple(Matcher<Ts> const&... ms) : matchers_(ms...) {}

  bool match(std::tuple<Ts...> const& input) const {
    bool success = true;
    base::tuple::for_each(
        [&success](auto const& m, auto const& val) { success &= m.match(val); },
        matchers_, input);
    return success;
  }

  std::string describe(bool positive) const override {
    if constexpr (sizeof...(Ts) == 0) {
      return positive ? "is an empty tuple" : "is not an empty tuple";
    } else if constexpr (sizeof...(Ts) == 1) {
      return "is a 1-element tuple whose element " +
             matchers_[0].describe(positive);
    } else {
      std::array<std::string, sizeof...(Ts)> descriptions;
      size_t index = 0;
      base::tuple::for_each(
          [&descriptions, &index, positive](auto const& m) {
            descriptions[index++] = m.describe(true);
          },
          matchers_);
      std::string result = "is a tuple where";
      if (!positive) { result += " one of the following is false:"; }
      for (size_t i = 0; i < sizeof...(Ts); ++i) {
        result += "\n                 parameter " + std::to_string(i) + " " +
                  descriptions[i];
      }
      return result;
    }
  }

 private:
  std::tuple<Matcher<Ts> const&...> matchers_;
};

template <typename T>
struct Eq : public Matcher<T> {
  Eq(T value) : value_(std::move(value)) {}
  bool match(T const& input) const override { return input == value_; }
  std::string describe(bool positive) const override {
    using base::stringify;
    return (positive ? "equals " : "does not equal ") + stringify(value_);
  }

  T value_;
};

template <int N>
Eq(char const (&)[N])->Eq<std::string>;

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
