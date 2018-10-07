#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <cstdio>
#include "base/string.h"

namespace base {
struct Logger {
  ~Logger() { fprintf(stderr, "\n"); }
};

template <typename T>
const Logger &operator<<(const Logger &l, const T &t) {
  fprintf(stderr, "%s", internal::stringify(t).c_str());
  return l;
}

}  // namespace base

#define LOG                                                                    \
  base::Logger{} << __FILE__ << ':' << __LINE__ << ' ' << __func__ << "] "

#endif  // ICARUS_BASE_LOG_H
