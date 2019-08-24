#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include "base/log.h"
#include "base/matchers.h"

#ifdef DBG
#define ICARUS_DEBUG

#define ICARUS_PRIVATE 

#define ASSERT(...)                                                            \
  static_cast<bool>((MATCH(::debug::Asserter{}, __VA_ARGS__))) ||              \
      base::Logger(                                                            \
          +[](std::experimental::source_location const &) -> std::string {     \
            return "";                                                         \
          },                                                                   \
          std::abort)

namespace debug {

struct Asserter {
  template <typename L, typename R>
  bool operator()(::matcher::ExprMatchResult<L, R> const &result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) const {
    if (!result.matched) {
      using base::stringify;
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31mAssertion failed\n"
             "    \033[0;1;37mExpected:\033[0m "
          << result.expr_string
          << "\n"
             "         \033[0;1;37mLHS:\033[0m "
          << stringify(result.lhs)
          << "\n"
             "         \033[0;1;37mRHS:\033[0m "
          << stringify(result.rhs) << "\n";
    }
    return result.matched;
  }

  template <typename T>
  bool operator()(::matcher::MatchResult<T> const &match_result,
                  std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) {
    if (match_result.description.has_value()) {
      using base::stringify;
      base::Logger(base::LogFormatterWithoutFunction, nullptr, src_loc)
          << "\033[0;1;31mAssertion failed\n"
             "  \033[0;1;37mExpression:\033[0m "
          << match_result.expr.string()
          << "\n"
             "    \033[0;1;37mExpected:\033[0m "
          << *match_result.description
          << "\n"
             "      \033[0;1;37mActual:\033[0m "
          << stringify(match_result.expr.value()) << "\n";
    }
    return !match_result.description.has_value();
  }
};
}  // namespace debug

#define NUM_ARGS(...)                                                          \
  INTERNAL_NUM_ARGS(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define INTERNAL_NUM_ARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, num, ...) num

#define DUMP(...)                                                              \
  [&] {                                                                        \
    using base::stringify;                                                     \
    return [&](auto &&... args) {                                              \
      return (std::string{} + ... + args);                                     \
    }(FOR_EACH(DUMP_, NUM_ARGS(__VA_ARGS__), __VA_ARGS__) "");                 \
  }()
#define DUMP_(arg) #arg " = " + stringify(arg) + ", ",

#define FOR_EACH(name, num, ...) BASE_INTERNAL_FOR_EACH(name, num, __VA_ARGS__)
#define BASE_INTERNAL_FOR_EACH(name, num, ...)                                 \
  BASE_APPLY_##num(name, __VA_ARGS__)
#define BASE_APPLY_1(macro, a) macro(a)
#define BASE_APPLY_2(macro, a, ...) macro(a) BASE_APPLY_1(macro, __VA_ARGS__)
#define BASE_APPLY_3(macro, a, ...) macro(a) BASE_APPLY_2(macro, __VA_ARGS__)
#define BASE_APPLY_4(macro, a, ...) macro(a) BASE_APPLY_3(macro, __VA_ARGS__)
#define BASE_APPLY_5(macro, a, ...) macro(a) BASE_APPLY_4(macro, __VA_ARGS__)
#define BASE_APPLY_6(macro, a, ...) macro(a) BASE_APPLY_5(macro, __VA_ARGS__)

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto &&ptr, std::experimental::source_location src_loc) {                \
    if (ptr == nullptr) {                                                      \
      ::base::Logger(::base::DefaultLogFormatter, std::abort, src_loc)         \
          << #expr " is unexpectedly null.";                                   \
    }                                                                          \
    return ptr;                                                                \
  })(expr, std::experimental::source_location::current())

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    DEBUG_LOG()                                                                \
    ("Unreachable code-path.\n", std::forward_as_tuple(__VA_ARGS__));          \
    std::abort();                                                              \
  } while (false)

#else

#define ICARUS_PRIVATE private:

#define ASSERT(...)                                                            \
  false && base::Logger(+[](std::experimental::source_location const &)        \
                            -> std::string { return ""; },                     \
                        nullptr)
#define ASSERT_NOT_NULL(...) __VA_ARGS__
#define UNREACHABLE(...) __builtin_unreachable();
#define DUMP(...) ""
#endif

#define NOT_YET(...)                                                           \
  do {                                                                         \
    DEBUG_LOG()("Not yet implemented.\n", std::forward_as_tuple(__VA_ARGS__)); \
    std::abort();                                                              \
  } while (false)

#endif  // ICARUS_BASE_DEBUG_H
