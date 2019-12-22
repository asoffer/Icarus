#include "module/pending.h"

#include "absl/container/node_hash_set.h"
#include "base/debug.h"
#include "base/macros.h"
#include "frontend/source/file.h"
#include "module/module.h"

namespace module {

namespace internal {
static absl::node_hash_set<frontend::CanonicalFileName> all_paths;

std::mutex mtx;

base::expected<std::pair<frontend::CanonicalFileName const *, bool>>
CanonicalizePath(frontend::FileName const &file_name) {
  ASSIGN_OR(return _.error(),  //
                   auto canonical_name,
                   frontend::CanonicalFileName::Make(file_name));
  auto [iter, newly_inserted] = all_paths.insert(std::move(canonical_name));
  return std::pair{&*iter, newly_inserted};
}

std::list<std::shared_future<BasicModule *>> pending_module_futures;
absl::node_hash_map<frontend::CanonicalFileName const *,
                    std::pair<std::shared_future<BasicModule *> *,
                              std::unique_ptr<BasicModule>>>
    all_modules;
}  // namespace internal

void AwaitAllModulesTransitively() {
  decltype(internal::pending_module_futures)::iterator iter, end_iter;
  {
    std::lock_guard lock(internal::mtx);
    iter     = internal::pending_module_futures.begin();
    end_iter = internal::pending_module_futures.end();
  }

  while (iter != end_iter) {
    iter->wait();
    std::lock_guard lock(internal::mtx);
    ++iter;
  }
}

}  // namespace module
