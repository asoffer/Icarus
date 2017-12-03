#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <iostream>
#include <functional>
#include "string.h"

namespace base {
struct Logger {
  ~Logger() { std::cerr << '\n'; }
};

template <typename T>
const Logger &operator<<(const Logger &l, const T &t) {
  std::cerr << internal::stringify(t);
  return l;
}

template <typename T> const Logger &operator,(const Logger &l, const T &t) {
  return l << t;
}


} // namespace base

#define LOG                                                                    \
  base::Logger{} << __FILE__ << ':' << __LINE__ << ' ' << __func__ << "] "

#endif // ICARUS_BASE_LOG_H
