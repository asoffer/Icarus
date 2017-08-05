#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <iostream>
#include <functional>

namespace base {
struct Logger {
  Logger(std::function<void()> fn) : fn_(std::move(fn)) {}
  ~Logger() {
    std::cerr << '\n';
    fn_();
  }
  private:
  std::function<void()> fn_;
};

template <typename T>
const Logger &operator<<(const Logger &l, const T &t) {
  std::cerr << t;
  return l;
}

template <typename T> const Logger &operator,(const Logger &l, const T &t) {
  std::cerr << t;
  return l;
}


} // namespace base

#define LOG_THEN(fn)                                                           \
  base::Logger{fn} << __FILE__ << ':' << __LINE__ << ' ' << __func__    \
                          << "] "
#define LOG LOG_THEN(+[]() {})

#endif // ICARUS_BASE_LOG_H
