#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#define ASSERT(cond, msg)                                                      \
  do {                                                                         \
    if (!(cond)) {                                                             \
      fprintf(stderr, "%s(%d): Assertion failed in %s.\n  %s\n%s\n", __FILE__, \
              __LINE__, __func__, #cond, msg);                                 \
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
