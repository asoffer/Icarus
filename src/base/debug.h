#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <cxxabi.h>
#include <signal.h>
#include <string>
#include <unistd.h>

#include "types.h"
#include "log.h"
#include "base/check.h"

#ifdef DBG
#define ASSERT(...)                                                            \
  do {                                                                         \
    if (!(::base::check::internal::LhsStealer(__FILE__, __LINE__,              \
                                              #__VA_ARGS__)                    \
          << __VA_ARGS__)) {                                                   \
      std::abort();                                                            \
    }                                                                          \
  } while (false)

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

#endif

#define NOT_YET(...)                                                           \
  do {                                                                         \
    auto logger = LOG << "Not yet implemented.\n";                             \
    debug::LogArgs(__VA_ARGS__);                                    \
    std::abort();                                                              \
  } while (false)

namespace debug {
template <typename... Args> void LogArgs(Args &&... args) {
  [[maybe_unused]] const auto &log =
      (base::Logger{} << ... << std::forward<Args>(args));
}
} // namespace debug

#ifdef DBG
#define AT(access) .at(access)
#else
#define AT(access) [access]
#endif

#endif // ICARUS_BASE_DEBUG_H
