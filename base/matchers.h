#ifndef ICARUS_BASE_MATCHERS_H
#define ICARUS_BASE_MATCHERS_H

#include <vector>
#include <optional>
#include <string>
#include <memory>

#include "base/stringify.h"
#include "base/tuple.h"

namespace matcher {

// A reference to a constant expression along with it's macro-stringification.
template <typename T>
struct Expression {
  T const& value() const& { return value_; }
  char const* string() const& { return string_; }

  Expression(char const* s, T const& value) : string_(s), value_(value) {}
 private:
  char const* string_ = nullptr;
  T const& value_;
};

template <typename T>
struct MatchResult {
  Expression<T> expr;
  std::optional<std::string> description;
};
template <typename T>
MatchResult(Expression<T>, std::optional<std::string>)->MatchResult<T>;

template <typename T>
struct Matcher {
  MatchResult<T> match_and_describe(Expression<T> const& input) const& {
    if (match(input.value())) { return MatchResult<T>{input, std::nullopt}; }
    return MatchResult<T>{input, describe(true)};
  }

  virtual std::string describe(bool positive) const = 0;
  virtual bool match(T const&) const                = 0;
};

template <typename CrtpDerived>
struct UntypedMatcher {
  template <typename T>
  MatchResult<T> match_and_describe(Expression<T> const& input) const& {
    return typename CrtpDerived::template Matcher<T>(
               *static_cast<CrtpDerived const*>(this))
        .match_and_describe(input);
  }
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
struct InheritsFrom : public UntypedMatcher<InheritsFrom<T>> {
  template <typename Expr>
  struct Matcher : public ::matcher::Matcher<Expr> {
    Matcher(InheritsFrom const&m) {}
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
};

template <bool B>
struct CastsTo : public UntypedMatcher<CastsTo<B>> {
  template <typename Expr>
  struct Matcher : public ::matcher::Matcher<Expr> {
    Matcher(CastsTo const&m) {}
    bool match(Expr const& input) const {
      return static_cast<bool>(input) == B;
    }
    std::string describe(bool positive) const override {
      return std::string("casts to ") + (positive == B ? "true" : "false");
    }
  };
};

template <typename T>
struct Holds : public UntypedMatcher<Holds<T>> {
  template <typename V>
  struct Matcher : public ::matcher::Matcher<V> {
    Matcher(Holds const&m) {}
    bool match(V const& input) const {
      return std::holds_alternative<T>(input);
    }
    std::string describe(bool positive) const override {
      return (positive ? "holds a " : "does not hold a ") +
             std::string(typeid(T).name());
    }
  };
};

template <typename T>
struct OrderedElementsAre : public UntypedMatcher<OrderedElementsAre<T>> {
  template <typename... Args>
  OrderedElementsAre(::matcher::Matcher<T> const& m, Args const&... args)
      : matchers_{m, args...} {}

  template <typename Expr>
  struct Matcher : public ::matcher::Matcher<Expr> {
    Matcher(OrderedElementsAre const& m) : m_(m) {}
    bool match(Expr const& input) const {
      auto it         = input.begin();
      auto matcher_it = m_.matchers_.begin();
      while (it != input.end() && matcher_it != m_.matchers_.end()) {
        if (!matcher_it->get().match(*it)) { 
          return false; }
        ++matcher_it;
        ++it;
      }
      return it == input.end() && matcher_it == m_.matchers_.end();
    }

    std::string describe(bool positive) const override {
      std::string result =
          (positive ? "Has" : "Does not have") +
          std::string(" the following elements in the following order:");
      for (auto m : m_.matchers_) {
        result += "\n                * " + m.get().describe(true);
      }
      return result;
    }

   private:
    OrderedElementsAre const& m_;
  };

 private:
  std::vector<std::reference_wrapper<::matcher::Matcher<T> const>> matchers_;
};

struct IsEmpty : public UntypedMatcher<IsEmpty> {
  template <typename V>
  struct Matcher : public ::matcher::Matcher<V> {
    Matcher(IsEmpty const&m) {}
    bool match(V const& input) const { return input.empty(); }
    std::string describe(bool positive) const override {
      return positive ? "is empty" : "is not empty";
    }
  };
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
  char const *expr_string = nullptr;
  bool matched;
  L const& lhs;
  R const& rhs;
};

namespace internal {
template <typename T>
struct StolenExpr {
  StolenExpr(char const* expr_string, T const& expr)
      : expr_string_(expr_string), expr_(expr) {}

  char const* expr_string_;
  T const& expr_;
};

struct ExprStealer {
  char const* expr_string_;
};

template <typename T>
StolenExpr<T> operator<<(ExprStealer expr_stealer, T const& expr) {
  return {expr_stealer.expr_string_, expr};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator==(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ == rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator!=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ != rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator<(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ < rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator>(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ > rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator<=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ <= rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

template <typename T, typename U>
ExprMatchResult<T, U> operator>=(StolenExpr<T> expr, U const& rhs) {
  bool matched = (expr.expr_ >= rhs);
  return {expr.expr_string_, matched, expr.expr_, rhs};
}

}  // namespace internal
}  // namespace matcher

#define MATCH(...)                                                             \
  INTERNAL_MATCH(                                                              \
      INTERNAL_MATCH_HANDLER_PICKER(__VA_ARGS__, INTERNAL_MATCH_MATCHERS,      \
                                    INTERNAL_MATCH_EXPR, _),                   \
      __VA_ARGS__)

#define INTERNAL_MATCH_HANDLER_PICKER(a, b, c, handler, ...) handler
#define INTERNAL_MATCH(handler, ...) handler(__VA_ARGS__)

#define INTERNAL_MATCH_MATCHERS(handler, expr, m)                              \
  handler((m).match_and_describe(::matcher::Expression{#expr, (expr)}))

#define INTERNAL_MATCH_EXPR(handler, expr)                                     \
  handler(::matcher::internal::ExprStealer{#expr} << expr)

#endif  // ICARUS_BASE_MATCHERS_H
