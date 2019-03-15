#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <experimental/source_location>
#include <iostream>
#include <mutex>
#include "base/stringify.h"

namespace base {
inline std::mutex logger_mtx_;

struct Logger {
  Logger(std::string (*fmt)(std::experimental::source_location const &src),
         void (*fn)() = nullptr,
         std::experimental::source_location src_loc =
             std::experimental::source_location::current())
      : fn_(fn) {
    logger_mtx_.lock();
    std::cerr << fmt(src_loc);
  }

  operator bool() const { return true; }

  // Loggers are not shared betweer threads, so this doesn't need to be
  // synchronized in any way.
  mutable bool locked_ = false;

  ~Logger() {
    logger_mtx_.unlock();
    std::cerr << "\n";
    if (fn_) { fn_(); }
  }
  void (*fn_)() = nullptr;
  bool do_log_ = false;
};

template <typename T>
Logger const &operator<<(Logger const &l, T const &t) {
  std::cerr << stringify(t);
  return l;
}

inline std::string LogFormatterWithoutFunction(
    std::experimental::source_location const &src_loc) {
  return std::string("\033[0;1;34m[") + src_loc.file_name() + ": " +
         std::to_string(src_loc.line()) + "] \033[0m";
}

inline std::string DefaultLogFormatter(
    std::experimental::source_location const &src_loc) {
  return std::string("\033[0;1;34m[") + src_loc.file_name() + ": " +
         std::to_string(src_loc.line()) + " " + src_loc.function_name() +
         "] \033[0m";
}

inline Logger Log(std::experimental::source_location src_loc =
                      std::experimental::source_location::current()) {
  return Logger{DefaultLogFormatter, nullptr, src_loc};
}

}  // namespace base

#endif  // ICARUS_BASE_LOG_H
