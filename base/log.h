#ifndef ICARUS_BASE_LOG_H
#define ICARUS_BASE_LOG_H

#include <experimental/source_location>
#include <string_view>

#include "absl/base/attributes.h"
#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/synchronization/mutex.h"

namespace base {

namespace internal_logging {

uintptr_t CurrentThreadId();

inline absl::Mutex mutex;
inline absl::flat_hash_map<std::string_view, std::vector<std::atomic<bool> *>>
    log_switches;
inline absl::flat_hash_set<std::string_view> on_logs;

ABSL_CONST_INIT inline absl::Mutex logger_mtx_(absl::kConstInit);

inline constexpr std::string_view kDefaultLogFormat =
    "\033[0;1;34m[%1$u %2$s:%3$u %4$s] \033[0m";
inline constexpr std::string_view kLogWithoutFunctionNameFormat =
    "\033[0;1;34m[%1$u %2$s:%3$u%4$0s] \033[0m";

template <typename... Args>
void Log(absl::FormatSpec<uintptr_t, std::string_view, size_t,
                          std::string_view> const &log_line_fmt,
         std::experimental::source_location loc, std::string_view fn_name,
         absl::FormatSpec<Args...> const &fmt, Args const &...args) {
  absl::MutexLock lock(&base::internal_logging::logger_mtx_);
  absl::FPrintF(stderr, log_line_fmt, CurrentThreadId(),
                std::string_view(loc.file_name()),
                static_cast<size_t>(loc.line()), fn_name);
  absl::FPrintF(stderr, fmt, args...);
}

}  // namespace internal_logging

void EnableLogging(std::string_view key);
void DisableLogging(std::string_view key);

#define LOG(k, fmt, ...)                                                       \
  do {                                                                         \
    static constexpr auto kFunc = __func__;                                    \
    if ([](std::string_view key) -> bool {                                     \
          static std::atomic<bool> is_on([key] {                               \
            if (std::string_view(key).empty()) { return true; }                \
            absl::MutexLock lock(&::base::internal_logging::mutex);            \
            bool log_on = ::base::internal_logging::on_logs.contains(key);     \
            ::base::internal_logging::log_switches[key].push_back(&is_on);     \
            return log_on;                                                     \
          }());                                                                \
          return is_on.load(std::memory_order_relaxed);                        \
        }(k)) {                                                                \
      [&](auto const &...args) {                                               \
        ::base::internal_logging::Log(                                         \
            ::base::internal_logging::kDefaultLogFormat,                       \
            ::std::experimental::source_location::current(), kFunc, fmt "\n",  \
            args...);                                                          \
      }(__VA_ARGS__);                                                          \
    }                                                                          \
  } while (false)

}  // namespace base

#endif  // ICARUS_BASE_LOG_H
