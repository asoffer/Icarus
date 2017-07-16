#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <string>

namespace debug {
std::string to_string(const char *s);
std::string to_string(std::string s);
inline std::string to_string(size_t n) { return std::to_string(n); }

template <typename T> std::string to_string(const T *ptr) {
  return std::to_string(ptr);
}
}

#define ASSERT(cond, msg)                                                      \
  do {                                                                         \
    if (!(cond)) {                                                             \
      fprintf(stderr, "%s(%d): Assertion failed in %s.\n  %s\n%s\n", __FILE__, \
              __LINE__, __func__, #cond, debug::to_string(msg).c_str());       \
      abort();                                                                 \
    }                                                                          \
  } while (false)

#define ASSERT_EQ(lhs, rhs)                                                    \
  do {                                                                         \
    if (!((lhs) == (rhs))) {                                                   \
      fprintf(stderr, "%s(%d): Assertion failed in %s.\n"                      \
                      "  Expected:\n"                                          \
                      "    %s == %s\n"                                         \
                      "  Actual:\n"                                            \
                      "    LHS = %s\n"                                         \
                      "    RHS = %s\n",                                        \
              __FILE__, __LINE__, __func__, #lhs, #rhs,                        \
              debug::to_string(lhs).c_str(), debug::to_string(rhs).c_str());   \
      abort();                                                                 \
    }                                                                          \
  } while (false)

#define ASSERT_NE(lhs, rhs)                                                    \
  do {                                                                         \
    if (!((lhs) != (rhs))) {                                                   \
      fprintf(stderr, "%s(%d): Assertion failed in %s.\n"                      \
                      "  Expected:\n"                                          \
                      "    %s != %s\n"                                         \
                      "  Actual:\n"                                            \
                      "    LHS = %s\n"                                         \
                      "    RHS = %s\n",                                        \
              __FILE__, __LINE__, __func__, #lhs, #rhs,                        \
              debug::to_string(lhs).c_str(), debug::to_string(rhs).c_str());   \
      abort();                                                                 \
    }                                                                          \
  } while (false)

#define NOT_YET ASSERT(false, "Not yet implemented")
#define UNREACHABLE ASSERT(false, "Unreachable code-path")

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
#endif

#endif // ICARUS_BASE_DEBUG_H
