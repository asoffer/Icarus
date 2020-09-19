#include "base/log.h"

namespace base {
namespace {

thread_local void const *const thread_id = &thread_id;

template <bool B>
void SetLogging(std::string_view key) {
  auto handle = ::base::internal_logging::log_switches.lock();
  if constexpr (B) {
    ::base::internal_logging::on_logs.lock()->insert(key);
  } else {
    ::base::internal_logging::on_logs.lock()->erase(key);
  }
  if (auto iter = handle->find(key); iter != handle->end()) {
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
