#include "module/pending.h"

#include <sstream>

#include "absl/container/node_hash_map.h"
#include "absl/container/node_hash_set.h"
#include "base/debug.h"
#include "base/macros.h"
#include "frontend/source/file.h"
#include "module/module.h"

namespace module {

namespace {

std::mutex mtx;
absl::node_hash_set<frontend::CanonicalFileName> all_paths;
std::list<std::shared_future<BasicModule *>> pending_module_futures;
absl::node_hash_map<frontend::CanonicalFileName const *,
                    std::pair<std::shared_future<BasicModule *> *,
                              std::unique_ptr<BasicModule>>>
    all_modules;
}  // namespace

static base::expected<std::pair<frontend::CanonicalFileName const *, bool>>
CanonicalizePath(frontend::FileName const &file_name) {
  ASSIGN_OR(return _.error(),  //
                   auto canonical_name,
                   frontend::CanonicalFileName::Make(file_name));
  auto [iter, newly_inserted] = all_paths.insert(std::move(canonical_name));
  return std::pair{&*iter, newly_inserted};
}

void AwaitAllModulesTransitively() {
  decltype(pending_module_futures)::iterator iter, end_iter;
  {
    std::lock_guard lock(mtx);
    iter     = pending_module_futures.begin();
    end_iter = pending_module_futures.end();
  }

  while (iter != end_iter) {
    iter->wait();
    // TODO I'm not sure this lock is necessary.
    std::lock_guard lock(mtx);
    ++iter;
  }
}

base::expected<Pending<BasicModule>> ImportModuleImpl(
    frontend::FileName const &file_name,
    std::unique_ptr<BasicModule> (*creator)()) {
  std::lock_guard lock(mtx);
  ASSIGN_OR(return _.error(), auto dependee, CanonicalizePath(file_name));
  auto [canonical_src, new_src] = dependee;

  // TODO Need to add dependencies even if the node was already scheduled (hence
  // the "already scheduled" check is done after this).
  //
  // TODO detect dependency cycles.

  auto[iter, inserted] = all_modules.try_emplace(canonical_src);
  auto & [ fut, mod ]  = iter->second;

  if (not new_src) { return Pending<BasicModule>{ASSERT_NOT_NULL(fut)}; }

  ASSERT(fut == nullptr);

  fut = &pending_module_futures.emplace_back(std::async(
      std::launch::async,
      [creator, canonical_src, mod(&iter->second.second)]() -> BasicModule * {
        // TODO error messages.
        ASSIGN_OR(return nullptr,  //
                         frontend::FileSource file_src,
                         frontend::FileSource::Make(*canonical_src));
        *mod = creator();
        (*mod)->ProcessFromSource(&file_src);
        return mod->get();
      }));
  return Pending<BasicModule>{fut};
}

}  // namespace module
