#include "base/log.h"

namespace base {
namespace internal {
base::guarded<
    absl::flat_hash_map<std::string_view, std::vector<std::atomic<bool> *>>>
    log_switches;
base::guarded<absl::flat_hash_set<std::string_view>> on_logs;
}  // namespace internal

void EnableLogging(std::string_view key) {
  auto handle = internal::log_switches.lock();
  internal::on_logs.lock()->insert(key);
  if (auto iter = handle->find(key); iter != handle->end()) {
    for (auto *ptr : iter->second) {
      // TODO determine the minimum correct memory ordering constraint.
      // TODO currently there is no mechanism for disabling logs so relocking
      // `on_logs` each time is overkill.
      *ptr = true;
    }
  }
}

}  // namespace base
