#include <any>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "base/stringify.h"

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
    auto&& m = (matcher);                                                      \
    auto&& e = (expr);                                                         \
    ++stats_.expectations;                                                     \
    if (m.match(e)) {                                                          \
      ++stats_.passes;                                                         \
    } else {                                                                   \
      /* TODO specify output logger? */                                        \
      std::cerr << "\n\033[0;1;34m[" __FILE__ ": " << std::to_string(__LINE__)   \
                << "]\033[0;1;31m Check failed\n"                              \
                   "  \033[0;1;37mExpression:\033[0m " #expr                   \
                   "\n"                                                        \
                   "    \033[0;1;37mExpected:\033[0m "                         \
                << m.describe(true) << "\n"                                    \
                << "      \033[0;1;37mActual:\033[0m " << expr << "\n";        \
    }                                                                          \
  } while (false)

struct Matcher {
  virtual Matcher* move()                           = 0;
  virtual bool match(std::any const& input) const   = 0;
  virtual std::string describe(bool positive) const = 0;
};

struct Not : public Matcher {
  virtual Matcher* move() { return new Not(std::move(*m_)); }
  Not(Matcher&& m) : m_(m.move()) {}
  bool match(std::any const& input) const override { return !m_->match(input); }
  std::string describe(bool positive) const override {
    return m_->describe(!positive);
  }

 private:
  std::unique_ptr<Matcher> m_;
};
inline Not operator!(Matcher&& m) { return Not{std::move(m)}; }

struct And : public Matcher {
  virtual Matcher* move() {
    return new And(std::move(*lhs_), std::move(*rhs_));
  }
  And(Matcher&& lhs, Matcher&& rhs) : lhs_(lhs.move()), rhs_(rhs.move()) {}
  bool match(std::any const& input) const override {
    return lhs_->match(input) && rhs_->match(input);
  }
  std::string describe(bool positive) const override {
    return lhs_->describe(positive) + " and " + rhs_->describe(positive);
  }

 private:
  std::unique_ptr<Matcher> lhs_, rhs_;
};

inline And operator&&(Matcher&& lhs, Matcher&& rhs) {
  return And{std::move(lhs), std::move(rhs)};
}

struct Or : public Matcher {
  virtual Matcher* move() { return new Or(std::move(*lhs_), std::move(*rhs_)); }
  Or(Matcher&& lhs, Matcher&& rhs) : lhs_(lhs.move()), rhs_(rhs.move()) {}
  bool match(std::any const& input) const override {
    return lhs_->match(input) || rhs_->match(input);
  }
  std::string describe(bool positive) const override {
    return lhs_->describe(positive) + " or " + rhs_->describe(positive);
  }

 private:
  std::unique_ptr<Matcher> lhs_, rhs_;
};

inline Or operator||(Matcher&& lhs, Matcher&& rhs) {
  return Or{std::move(lhs), std::move(rhs)};
}

template <typename T>
struct Eq : public test::Matcher {
  virtual test::Matcher* move() { return new Eq<T>(std::move(value_)); }
  Eq(T value) : value_(std::move(value)) {}
  bool match(std::any const& input) const override {
    if (auto* t = std::any_cast<T>(&input)) { return *t == value_; }
    return false;
  }
  std::string describe(bool positive) const override {
    return (positive ? "equals " : "does not equal ") + base::stringify(value_);
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
