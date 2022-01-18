#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include "base/log.h"
#include "base/universal_print.h"

#if defined(ICARUS_DEBUG)

#define ICARUS_DEBUG_ONLY(...) __VA_ARGS__

#define ICARUS_CONSTEXPR inline
#define ICARUS_PRIVATE

#define ASSERT(expr)                                                           \
  do {                                                                         \
    if (not ::debug::Asserter{}(::debug::internal_matcher::ExprStealer{#expr}  \
                                << expr)) {                                    \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

namespace debug {
namespace internal_matcher {

template <typename L, typename R>
struct ExprMatchResult {
  char const* expr_string = nullptr;
  bool matched;
  L const& lhs;
  R const& rhs;
};

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

}  // namespace internal_matcher

struct Asserter {
  template <typename L, typename R>
  bool operator()(
      ::debug::internal_matcher::ExprMatchResult<L, R> const& result,
      std::experimental::source_location src_loc =
          std::experimental::source_location::current()) const {
    if (not result.matched) {
      base::internal_logging::Log(
          ::base::internal_logging::kLogWithoutFunctionNameFormat, src_loc, "",
          "\033[0;1;31mAssertion failed\n"
          "    \033[0;1;37mExpected:\033[0m %s\n"
          "         \033[0;1;37mLHS:\033[0m %s\n"
          "         \033[0;1;37mRHS:\033[0m %s\n",
          result.expr_string, ::base::UniversalPrintToString(result.lhs),
          ::base::UniversalPrintToString(result.rhs));
    }
    return result.matched;
  }
};

}  // namespace debug

// TODO: On both of the calls below, we could improve performance. Rather than
// UniversalPrintToString, we could add a LogPrinter that printed directly to
// the log without the intermediate string type.

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto&& ptr,                                                              \
      std::experimental::source_location src_loc) -> decltype(auto) {          \
    if (ptr == nullptr) {                                                      \
      LOG("", "%s is unexpectedly null.", #expr);                              \
      ::std::abort();                                                          \
    }                                                                          \
    return static_cast<std::remove_reference_t<decltype(ptr)>&&>(ptr);         \
  })(expr, std::experimental::source_location::current())

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    LOG("", "Unreachable code-path.\n%s",                                      \
        ::base::UniversalPrintToString(std::forward_as_tuple(__VA_ARGS__)));   \
    std::abort();                                                              \
  } while (false)

#else  // defined(ICARUS_DEBUG)

#define ICARUS_DEBUG_ONLY(...)

#define ICARUS_CONSTEXPR constexpr
#define ICARUS_PRIVATE private:

#define ASSERT(...) static_assert(true)
#define ASSERT_NOT_NULL(...) (__VA_ARGS__)
#define UNREACHABLE(...) __builtin_unreachable();

#endif  // defined(ICARUS_DEBUG)

#define NOT_YET(...)                                                           \
  do {                                                                         \
    LOG("", "Not yet implemented.\n%s",                                        \
        ::base::UniversalPrintToString(std::forward_as_tuple(__VA_ARGS__)));   \
    std::abort();                                                              \
  } while (false)

#endif  // ICARUS_BASE_DEBUG_H
