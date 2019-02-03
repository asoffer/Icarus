#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <cstdio>
#include "base/string.h"

namespace base {
struct Logger {
  Logger() = default;
  Logger(void (*fn)()) : fn_(fn) {}

  operator bool () const { return true; }

  ~Logger() {
    fprintf(stderr, "\n");
    if (fn_) { fn_(); }
  }
  void (*fn_)() = nullptr;
};

template <typename T>
Logger const &operator<<(Logger const &l, T const &t) {
  fprintf(stderr, "%s", internal::stringify(t).c_str());
  return l;
}

}  // namespace base

#define LOG                                                                    \
  ::base::Logger{} << __FILE__ << ':' << __LINE__ << ' ' << __func__ << "] "

#endif  // ICARUS_BASE_LOG_H
