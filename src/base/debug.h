#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <string>

#include "types.h"
#include "log.h"

namespace debug {
inline std::string to_string(const char *s) { return std::string(s); }
inline std::string to_string(std::string s) { return s; }
inline std::string to_string(std::nullptr_t) { return "nullptr"; }
inline std::string to_string(i64 n) { return std::to_string(n); }
inline std::string to_string(u64 n) { return std::to_string(n); }
inline std::string to_string(i32 n) { return std::to_string(n); }
inline std::string to_string(u32 n) { return std::to_string(n); }
template <typename T> std::string to_string(const T *ptr) {
  return std::to_string(ptr);
}
}

#define ASSERT(cond, msg)                                                      \
  do {                                                                         \
    if (!(cond)) {                                                             \
      LOG << "Assertion failed.\n"                                             \
          << #cond << '\n'                                                     \
          << "  " << debug::to_string(msg);                                    \
      abort(); /* So compiler doesn't complain about missing return */         \
    }                                                                          \
  } while (false)

#define ASSERT_SYM(lhs, rhs, sym)                                              \
  do {                                                                         \
    if (!((lhs)sym(rhs))) {                                                    \
      LOG << "Assertion failed.\n"                                             \
             "  Expected: "                                                    \
          << #lhs << ' ' << #sym << ' ' << #rhs << "\n  Actual:\n"             \
          << "    LHS = " << debug::to_string(lhs) << '\n'                     \
          << "    RHS = " << debug::to_string(rhs) << '\n';                    \
      abort(); /* So compiler doesn't complain about missing return */         \
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
          << "  Actual type: " << val->to_string();                            \
    }                                                                          \
  } while (false)

#define NOT_YET(...)                                                           \
  LOG << "Not yet implemented.\n", ##__VA_ARGS__;                              \
  abort()

#define UNREACHABLE(...)                                                       \
  LOG << "Unreachable code-path.\n", ##__VA_ARGS__;                            \
  abort()

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif

#endif // ICARUS_BASE_DEBUG_H
