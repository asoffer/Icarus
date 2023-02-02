#include "base/log.h"

namespace base {
namespace {

thread_local void const *const thread_id = &thread_id;

template <bool B>
void SetLogging(std::string_view key) {
  absl::MutexLock lock(&::base::internal_logging::mutex);
  if constexpr (B) {
    ::base::internal_logging::on_logs.insert(key);
  } else {
    ::base::internal_logging::on_logs.erase(key);
  }
  if (auto iter = ::base::internal_logging::log_switches.find(key);
      iter != ::base::internal_logging::log_switches.end()) {
    for (auto *ptr : iter->second) { ptr->store(B, std::memory_order_relaxed); }
  }
}

}  // namespace

void EnableLogging(std::string_view key) { SetLogging<true>(key); }
void DisableLogging(std::string_view key) { SetLogging<false>(key); }

namespace internal_logging {

uintptr_t CurrentThreadId() { return reinterpret_cast<uintptr_t>(thread_id); }

}  // namespace internal_logging

}  // namespace base
