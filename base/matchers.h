#include <optional>
#include <string>
#include <memory>

#include "base/stringify.h"
#include "base/tuple.h"

namespace matcher {
struct UntypedMatcher {};

template <typename T>
struct Matcher : public UntypedMatcher {
  std::optional<std::string> match_and_describe(T const& input) const& {
    if (match(input)) { return std::nullopt; }
    return describe(true);
  }

  template <typename Expr>
  auto const& With() const& {
    return *this;
  }

  virtual std::string describe(bool positive) const = 0;
  virtual bool match(T const&) const                = 0;
};

namespace internal {
template <typename T>
struct is_pointery : public std::false_type {};

template <typename T>
struct is_pointery<T*> : public std::true_type {
  T const* get(T const* ptr) { return ptr; }
};

template <typename T>
struct is_pointery<std::unique_ptr<T>> : public std::true_type {
  T const* get(std::unique_ptr<T> const& ptr) { return ptr.get(); }
};
}  // namespace internal

template <typename T>
struct InheritsFrom : public UntypedMatcher {
  template <typename Expr>
  struct Matcher : public ::matcher::Matcher<Expr> {
    bool match(Expr const& input) const {
      if constexpr (internal::is_pointery<Expr>::value) {
        return dynamic_cast<T const*>(
                   internal::is_pointery<Expr>{}.get(input)) != nullptr;
      }
    }
    std::string describe(bool positive) const override {
      return (positive ? "inherits from " : "does not inherit from ") +
             std::string(typeid(T).name());
    }
  };

  template <typename Expr>
  auto With() const& {
    return InheritsFrom::Matcher<Expr>{};
  }
};

template <typename T>
struct Holds : public UntypedMatcher {
  template <typename V>
  struct Matcher : public ::matcher::Matcher<V> {
    bool match(V const& input) const {
      return std::holds_alternative<T>(input);
    }
    std::string describe(bool positive) const override {
      return (positive ? " holds a " : "does not hold a ") +
             std::string(typeid(T).name());
    }
  };

  template <typename Expr>
  auto With() const& {
    return Holds::Matcher<Expr>{};
  }
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
    using ::base::stringify;
    return (positive ? "equals " : "does not equal ") + stringify(value_);
  }

  T value_;
};

template <int N>
Eq(char const (&)[N])->Eq<std::string>;

template <typename L, typename R>
struct ExprMatchResult {
  bool matched;
  L const& lhs;
  R const& rhs;
};

namespace internal {
template <typename T>
struct StolenExpr {
  StolenExpr(T const& expr) : expr_(expr) {}

  T const& expr_;
};

struct ExprStealer {};
template <typename T>
StolenExpr<T> operator<<(ExprStealer, T const& expr) {
  return {expr};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator==(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ == rhs);
  return {matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator!=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ != rhs);
  return {matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator<(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ < rhs);
  return {matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator>(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ > rhs);
  return {matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator<=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ <= rhs);
  return {matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator>=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ >= rhs);
  return {matched, expr.expr_, rhs};
}

}  // namespace internal
}  // namespace matcher

#define MATCH(matcher_macro, expr_macro, ...)                                  \
  MATCH_IMPL(MATCH_IMPL_NUM_ARGS(__VA_ARGS__, matcher_macro, expr_macro, _),   \
             __VA_ARGS__)
#define MATCH_IMPL_NUM_ARGS(x, y, macro, ...) macro
#define MATCH_IMPL(match, ...) match(__VA_ARGS__)

#define MATCH_EXPR(expr) (::matcher::internal::ExprStealer{} << expr)
