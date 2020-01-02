#ifndef ICARUS_BASE_MATCHERS_H
#define ICARUS_BASE_MATCHERS_H

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "base/stringify.h"
#include "base/tuple.h"

namespace matcher {

// A reference to a constant expression along with it's macro-stringification.
template <typename T>
struct Expression {
  T const& value() const & { return value_; }
  char const* string() const & { return string_; }

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
  MatchResult<T> match_and_describe(Expression<T> const& input) const & {
    if (match(input.value())) { return MatchResult<T>{input, std::nullopt}; }
    return MatchResult<T>{input, describe(true)};
  }

  virtual std::string describe(bool positive) const = 0;
  virtual bool match(T const&) const                = 0;
};

template <typename CrtpDerived>
struct UntypedMatcher {
  template <typename T>
  MatchResult<T> match_and_describe(Expression<T> const& input) const & {
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
    Matcher(InheritsFrom const& m) {}
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

template <typename L, typename R>
struct ExprMatchResult {
  char const* expr_string = nullptr;
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
