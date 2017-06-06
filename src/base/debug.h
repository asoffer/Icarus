#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <string>

namespace debug {
std::string to_string(const char *s);
std::string to_string(std::string s);
}

#define ASSERT(cond, msg)                                                      \
  do {                                                                         \
    if (!(cond)) {                                                             \
      fprintf(stderr, "%s(%d): Assertion failed in %s.\n  %s\n%s\n", __FILE__, \
              __LINE__, __func__, #cond, debug::to_string(msg).c_str());       \
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

template <typename To, typename From> To *ptr_cast(From* ptr) {
#ifdef DEBUG
  auto result = dynamic_cast<To*>(ptr);
  ASSERT(result, "Failed to convert");
  return result;
#else
  return static_cast<To*>(ptr);
#endif
}

#endif // ICARUS_BASE_DEBUG_H
