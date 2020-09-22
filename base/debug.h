#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include "base/log.h"
#include "base/matchers.h"

#if defined(ICARUS_DEBUG)

#define ICARUS_DEBUG_ONLY(...) __VA_ARGS__

#define ICARUS_CONSTEXPR inline
#define ICARUS_PRIVATE

#define ASSERT(...)                                                            \
  do {                                                                         \
    if (not static_cast<bool>(MATCH(::debug::Asserter{}, __VA_ARGS__))) {      \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

namespace debug {

struct Asserter {
  template <typename L, typename R>
  bool operator()(::matcher::ExprMatchResult<L, R> const &result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) const {
    if (not result.matched) {
      using ::base::stringify;
      base::internal_logging::Log(
          ::base::internal_logging::kLogWithoutFunctionNameFormat, src_loc, "",
          "\033[0;1;31mAssertion failed\n"
          "    \033[0;1;37mExpected:\033[0m %s\n"
          "         \033[0;1;37mLHS:\033[0m %s\n"
          "         \033[0;1;37mRHS:\033[0m %s\n",
          result.expr_string, stringify(result.lhs), stringify(result.rhs));
    }
    return result.matched;
  }

  template <typename T>
  bool operator()(::matcher::MatchResult<T> const &match_result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) {
    if (match_result.description.has_value()) {
      using ::base::stringify;
      base::internal_logging::Log(
          ::base::internal_logging::kLogWithoutFunctionNameFormat, src_loc, "",
          "\033[0;1;31mAssertion failed\n"
          "  \033[0;1;37mExpression:\033[0m %s\n"
          "    \033[0;1;37mExpected:\033[0m %s\n"
          "      \033[0;1;37mActual:\033[0m %s\n",
          match_result.expr.string(), *match_result.description,
          stringify(match_result.expr.value()));
    }
    return not match_result.description.has_value();
  }
};
}  // namespace debug

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto &&ptr,                                                              \
      std::experimental::source_location src_loc) -> decltype(auto) {          \
    if (ptr == nullptr) {                                                      \
      LOG("", "%s is unexpectedly null.", #expr);                              \
      ::std::abort();                                                          \
    }                                                                          \
    return static_cast<std::remove_reference_t<decltype(ptr)> &&>(ptr);        \
  })(expr, std::experimental::source_location::current())

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    using ::base::stringify;                                                   \
    LOG("", "Unreachable code-path.\n%s",                                      \
        stringify(std::forward_as_tuple(__VA_ARGS__)));                        \
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
    using ::base::stringify;                                                   \
    LOG("", "Not yet implemented.\n%s",                                        \
        stringify(std::forward_as_tuple(__VA_ARGS__)));                        \
    std::abort();                                                              \
  } while (false)

#endif  // ICARUS_BASE_DEBUG_H
