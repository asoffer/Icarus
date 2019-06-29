#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <experimental/source_location>
#include <iostream>
#include <mutex>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "base/guarded.h"
#include "base/stringify.h"

namespace base {
namespace internal {
extern base::guarded<
    absl::flat_hash_map<std::string_view, std::vector<std::atomic<bool> *>>>
    log_switches;
extern base::guarded<absl::flat_hash_set<std::string_view>> on_logs;

inline std::recursive_mutex logger_mtx_;
}  // namespace base_internal

void EnableLogging(std::string_view key);

struct Logger {
  Logger(std::string (*fmt)(std::experimental::source_location const &src),
         void (*fn)() = nullptr,
         std::experimental::source_location src_loc =
             std::experimental::source_location::current())
      : fn_(fn) {
        internal::logger_mtx_.lock();
    std::cerr << fmt(src_loc);
  }

  operator bool() const { return true; }

  // Loggers are not shared betweer threads, so this doesn't need to be
  // synchronized in any way.
  mutable bool locked_ = false;

  ~Logger() {
    internal::logger_mtx_.unlock();
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

template <typename... Ts>
void LogForMacro(Logger const &log, Ts &&... ts) {
  (log << ... << std::forward<Ts>(ts));
}

#ifdef DBG
#define DEBUG_LOG(...)                                                         \
  if ([]() -> bool {                                                           \
        static std::atomic<bool> log_switch(false);                            \
        static int registration = [] {                                         \
          for (std::string_view key :                                          \
               std::initializer_list<std::string_view>{__VA_ARGS__}) {         \
            auto handle = ::base::internal::log_switches.lock();               \
            if (::base::internal::on_logs.lock()->contains(key)) {             \
              log_switch = true;                                               \
            };                                                                 \
            (*handle)[key].push_back(&log_switch);                             \
          }                                                                    \
          return 0;                                                            \
        }();                                                                   \
        return log_switch;                                                     \
      }()) {                                                                   \
  DEBUG_LOG_IMPL

#define DEBUG_LOG_IMPL(...)                                                    \
  LogForMacro(base::Logger{base::DefaultLogFormatter, nullptr,                 \
                           std::experimental::source_location::current()},     \
              __VA_ARGS__);                                                    \
  }                                                                            \
  do {                                                                         \
  } while (false)

#else
#define DEBUG_LOG(...) DEBUG_LOG_IMPL
#define DEBUG_LOG_IMPL(...)
#endif

}  // namespace base

#endif  // ICARUS_BASE_LOG_H
