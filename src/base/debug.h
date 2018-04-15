#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <cxxabi.h>
#include <signal.h>
#include <string>
#include <unistd.h>

#include "types.h"
#include "log.h"

#define ASSERT(cond, msg)                                                      \
  do {                                                                         \
    if (!(cond)) {                                                             \
      LOG << "Assertion failed.\n" << #cond << '\n' << "  " << (msg);          \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

#define ASSERT_SYM(lhs, rhs, sym)                                              \
  do {                                                                         \
    auto lhs_val = (lhs);                                                      \
    auto rhs_val = (rhs);                                                      \
    if (!(lhs_val sym rhs_val)) {                                              \
      LOG << "Assertion failed.\n"                                             \
             "  Expected: "                                                    \
          << #lhs << ' ' << #sym << ' ' << #rhs << "\n  Actual:\n"             \
          << "    LHS = " << lhs_val << '\n'                                   \
          << "    RHS = " << rhs_val << '\n';                                  \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

#define ASSERT_EQ(lhs, rhs) ASSERT_SYM(lhs, rhs, ==)
#define ASSERT_NE(lhs, rhs) ASSERT_SYM(lhs, rhs, !=)
#define ASSERT_GT(lhs, rhs) ASSERT_SYM(lhs, rhs, >)
#define ASSERT_GE(lhs, rhs) ASSERT_SYM(lhs, rhs, >=)
#define ASSERT_LT(lhs, rhs) ASSERT_SYM(lhs, rhs, <)
#define ASSERT_LE(lhs, rhs) ASSERT_SYM(lhs, rhs, <=)

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto *ptr) {                                                             \
    if (!ptr) { std::abort(); }                                                \
    return ptr;                                                                \
  })(expr)

#define ASSERT_TYPE(type, val)                                                 \
  do {                                                                         \
    if (!(val)->is<type>()) {                                                  \
      LOG << "Assertion failed.\n"                                             \
             "  Expected type: "                                               \
          << #type << "\n"                                                     \
          << "  Actual type: " << val;                                         \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

#define NOT_YET(...)                                                           \
  do {                                                                         \
    auto logger = LOG << "Not yet implemented.\n";                             \
    debug::LogArgs(__VA_ARGS__);                                    \
    std::abort();                                                              \
  } while (false)

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    auto logger = LOG << "Unreachable code-path.\n";                           \
    debug::LogArgs(__VA_ARGS__);                                               \
    std::abort();                                                              \
  } while (false)

namespace debug {
template <typename... Args> void LogArgs(Args &&... args) {
  [[maybe_unused]] const auto &log =
      (base::Logger{} << ... << std::forward<Args>(args));
}
} // namespace debug

#ifdef DBG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif

#endif // ICARUS_BASE_DEBUG_H
