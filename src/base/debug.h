#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <string>

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
    if (!((lhs)sym(rhs))) {                                                    \
      LOG << "Assertion failed.\n"                                             \
             "  Expected: "                                                    \
          << #lhs << ' ' << #sym << ' ' << #rhs << "\n  Actual:\n"             \
          << "    LHS = " << (lhs) << '\n'                                     \
          << "    RHS = " << (rhs) << '\n';                                    \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

#define ASSERT_EQ(lhs, rhs) ASSERT_SYM(lhs, rhs, ==)
#define ASSERT_NE(lhs, rhs) ASSERT_SYM(lhs, rhs, !=)
#define ASSERT_GT(lhs, rhs) ASSERT_SYM(lhs, rhs, >)
#define ASSERT_GE(lhs, rhs) ASSERT_SYM(lhs, rhs, >=)
#define ASSERT_LT(lhs, rhs) ASSERT_SYM(lhs, rhs, <)
#define ASSERT_LE(lhs, rhs) ASSERT_SYM(lhs, rhs, <=)

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

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif

#define WARN_ONCE(msg)                                                         \
  do {                                                                         \
    static bool warned = false;                                                \
    if (!warned) {                                                             \
      warned = true;                                                           \
      LOG << "Warning: " << msg;                                               \
    }                                                                          \
  } while (false)

#endif // ICARUS_BASE_DEBUG_H
