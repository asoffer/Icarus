#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include "base/check.h"
#include "base/log.h"

#ifdef DBG
#define ASSERT(...)                                                            \
  do {                                                                         \
    if (!(::base::check::internal::LhsStealer(__FILE__, __LINE__,              \
                                              #__VA_ARGS__)                    \
          << __VA_ARGS__)) {                                                   \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

#define NUM_ARGS(...)                                                          \
  INTERNAL_NUM_ARGS(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define INTERNAL_NUM_ARGS(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, num, ...) num

#define DUMP(...)                                                              \
  [&](auto &&... args) {                                                       \
    return (std::string{} + ... + args);                                       \
  }(FOR_EACH(DUMP_, NUM_ARGS(__VA_ARGS__), __VA_ARGS__) "")
#define DUMP_(arg) #arg " = " + ::base::internal::stringify(arg) + ", ",

#define FOR_EACH(name, num, ...) BASE_INTERNAL_FOR_EACH(name, num, __VA_ARGS__)
#define BASE_INTERNAL_FOR_EACH(name, num, ...) BASE_APPLY_##num(name, __VA_ARGS__)
#define BASE_APPLY_1(macro, a) macro(a)
#define BASE_APPLY_2(macro, a, ...) macro(a) BASE_APPLY_1(macro, __VA_ARGS__)
#define BASE_APPLY_3(macro, a, ...) macro(a) BASE_APPLY_2(macro, __VA_ARGS__)
#define BASE_APPLY_4(macro, a, ...) macro(a) BASE_APPLY_3(macro, __VA_ARGS__)
#define BASE_APPLY_5(macro, a, ...) macro(a) BASE_APPLY_4(macro, __VA_ARGS__)
#define BASE_APPLY_6(macro, a, ...) macro(a) BASE_APPLY_5(macro, __VA_ARGS__)

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto &&ptr) {                                                            \
    if (ptr == nullptr) {                                                      \
      LOG << #expr " is unexpectedly null.";                                   \
      std::abort();                                                            \
    }                                                                          \
    return ptr;                                                                \
  })(expr)

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    auto logger = LOG << "Unreachable code-path.\n";                           \
    debug::LogArgs(__VA_ARGS__);                                               \
    std::abort();                                                              \
  } while (false)

#else

#define ASSERT(...)
#define ASSERT_NOT_NULL(...) __VA_ARGS__
#define UNREACHABLE(...) __builtin_unreachable();
#define DUMP(...) ""
#endif

#define NOT_YET(...)                                                           \
  do {                                                                         \
    auto logger = LOG << "Not yet implemented.\n";                             \
    debug::LogArgs(__VA_ARGS__);                                               \
    std::abort();                                                              \
  } while (false)

namespace debug {
template <typename... Args>
void LogArgs(Args &&... args) {
  [[maybe_unused]] const auto &log =
      (base::Logger{} << ... << std::forward<Args>(args));
}
}  // namespace debug
#endif  // ICARUS_BASE_DEBUG_H
