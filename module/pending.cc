#include "module/pending.h"

#include <sstream>

#include "absl/container/node_hash_set.h"
#include "base/debug.h"
#include "base/graph.h"
#include "base/macros.h"
#include "frontend/source/file.h"
#include "module/module.h"

namespace module {
namespace {
struct PathHasher {
  size_t operator()(std::filesystem::path const &p) const {
    return std::filesystem::hash_value(p);
  }
};

std::mutex mtx;
absl::node_hash_set<std::filesystem::path, PathHasher> all_paths;
base::Graph<std::filesystem::path const *> import_dep_graph;
std::list<std::shared_future<Module *>> pending_module_futures;
absl::flat_hash_map<
    std::filesystem::path const *,
    std::pair<std::shared_future<Module *> *, std::unique_ptr<Module>>>
    all_modules;
}  // namespace

static base::expected<std::pair<std::filesystem::path const *, bool>>
CanonicalizePath(std::filesystem::path const &p) {
  std::error_code ec;
  auto canonical_path = std::filesystem::canonical(p, ec);
  std::stringstream ss;
  if (ec) {
    ss << ec;
    return base::unexpected{ss.str()};
  }
  auto [iter, newly_inserted] = all_paths.insert(std::move(canonical_path));
  return std::pair{&*iter, newly_inserted};
}

Module *PendingModule::get() {
  if ((data_ & 1) == 0) { return reinterpret_cast<Module *>(data_); }
  Module *result =
      reinterpret_cast<std::shared_future<Module *> *>(data_ - 1)->get();
  *this = PendingModule{result};
  return result;
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
    std::lock_guard lock(mtx);  // TODO I'm not sure this lock is necessary.
    ++iter;
  }
}

base::expected<PendingModule> ImportModule(std::filesystem::path const &src,
                                           Module const *requestor,
                                           Module *(*fn)(Module *)) {
  std::lock_guard lock(mtx);
  ASSIGN_OR(return _.error(), auto dependee, CanonicalizePath(src));
  auto &[canonical_src, new_src] = dependee;

  // TODO Need to add dependencies even if the node was already scheduled (hence
  // the "already scheduled" check is done after this).
  //
  // TODO detect dependency cycles.

  auto &[fut, mod] = all_modules[canonical_src];
  if (!new_src) { return PendingModule{ASSERT_NOT_NULL(fut)}; }

  ASSERT(fut == nullptr);

  // TODO this is wrong because the file source will die before the module is
  // compiled (asynchronously).
  ASSIGN_OR(return _.error(), frontend::FileSource file_src,
                   frontend::FileSource::Make(src));
  mod = std::make_unique<Module>(&file_src);

  fut = &pending_module_futures.emplace_back(
      std::async(std::launch::async, fn, mod.get()));
  return PendingModule{fut};
}

}  // namespace module
