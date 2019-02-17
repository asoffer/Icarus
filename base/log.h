#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <cstdio>
#include <mutex>
#include "base/stringify.h"

namespace base {
inline std::mutex logger_mtx_;
struct Logger {
  Logger() = default;
  Logger(void (*fn)()) : fn_(fn) {}

  operator bool () const { return true; }

  // Loggers are not shared betweer threads, so this doesn't need to be
  // synchronized in any way.
  mutable bool locked_ = false; 

  ~Logger() {
    if (locked_) { logger_mtx_.unlock(); }
    fprintf(stderr, "\n");
    if (fn_) { fn_(); }
  }
  void (*fn_)() = nullptr;
};

template <typename T>
Logger const &operator<<(Logger const &l, T const &t) {
  if (!l.locked_) { logger_mtx_.lock(); }
  l.locked_ = true;
  fprintf(stderr, "%s", stringify(t).c_str());
  return l;
}

}  // namespace base

#define LOG                                                                    \
  ::base::Logger{} << __FILE__ << ':' << __LINE__ << ' ' << __func__ << "] "

#endif  // ICARUS_BASE_LOG_H
