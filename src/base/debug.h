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

#include <execinfo.h>

namespace debug {
inline void DumpStackTrace(int) {
  constexpr u32 max_frames = 20;
  fprintf(stderr, "stack trace:\n");
  void *addrlist[max_frames + 1];
  int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void *));
  if (addrlen == 0) {
    fprintf(stderr, "  \n");
  } else {
    char **symbollist = backtrace_symbols(addrlist, addrlen);
    for (int i = 4; i < addrlen; i++) {
      std::string symbol = symbollist[i];
      auto start_iter    = symbol.find('(');
      auto end_iter      = symbol.find(')');
      std::string mangled =
          symbol.substr(start_iter + 1, end_iter - start_iter - 1);
      end_iter = mangled.find('+');
      mangled =
          end_iter >= mangled.size() ? mangled : mangled.substr(0, end_iter);
      char demangled[1024];
      size_t demangled_size = 1024;

      int status;
      abi::__cxa_demangle(mangled.c_str(), &demangled[0], &demangled_size,
                          &status);
      if (status != 0) {
        fprintf(stderr, "#%2d| %s\n", i - 3, symbol.c_str());
      } else {
        fprintf(stderr, "#%2d| %s\n", i - 3, demangled);
      }

    }
    free(symbollist);
  }
}
} // namespace debug

#endif // ICARUS_BASE_DEBUG_H
