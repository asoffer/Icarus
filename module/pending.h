#ifndef ICARUS_MODULE_PENDING_H
#define ICARUS_MODULE_PENDING_H

#include <future>
#include <list>
#include <mutex>
#include <utility>

#include "absl/container/node_hash_map.h"
#include "base/expected.h"
#include "base/macros.h"
#include "frontend/source/file.h"
#include "frontend/source/file_name.h"
#include "frontend/source/source.h"
#include "module.h"

namespace module {

template <typename ModType>
struct Pending;

// A `Pending<ModType>` represents a module that is currently being loaded but
// for which the data may not yet be available.
template <typename ModType>
struct Pending {
 public:
  // Returns the compiled module, possibly blocking if `get` is called before
  // the module has finished compiling.
  ModType *get() {
    if ((data_ & 1) == 0) { return reinterpret_cast<ModType *>(data_); }
    ModType *result = reinterpret_cast<ModType *>(
        reinterpret_cast<std::shared_future<BasicModule *> *>(data_ - 1)
            ->get());
    *this = Pending{result};
    return result;
  }

  bool valid() const { return data_ != 0; }

 private:
  // TODO restrict friendship to just the relevant instantiation.
  template <typename M>
  friend base::expected<Pending<M>> ImportModule(
      frontend::FileName const &file_name);

  uintptr_t data_ = 0;

  Pending() = default;
  explicit Pending(ModType *mod) : data_(reinterpret_cast<uintptr_t>(mod)) {}
  explicit Pending(std::shared_future<BasicModule *> *mod)
      : data_(reinterpret_cast<uintptr_t>(mod) | 0x01) {}
};

void AwaitAllModulesTransitively();

namespace internal {

extern std::mutex mtx;
extern std::list<std::shared_future<BasicModule *>> pending_module_futures;
extern absl::node_hash_map<frontend::CanonicalFileName const *,
                           std::pair<std::shared_future<BasicModule *> *,
                                     std::unique_ptr<BasicModule>>>
    all_modules;

base::expected<std::pair<frontend::CanonicalFileName const *, bool>>
CanonicalizePath(frontend::FileName const &file_name);

}  // namespace internal

template <typename ModType>
base::expected<Pending<ModType>> ImportModule(
    frontend::FileName const &file_name) {
  std::lock_guard lock(internal::mtx);
  ASSIGN_OR(return _.error(),  //
                   auto dependee, internal::CanonicalizePath(file_name));
  auto [canonical_src, new_src] = dependee;

  // TODO Need to add dependencies even if the node was already scheduled (hence
  // the "already scheduled" check is done after this).
  //
  // TODO detect dependency cycles.

  auto [iter, inserted] = internal::all_modules.try_emplace(canonical_src);
  auto &[fut, mod]      = iter->second;

  if (not new_src) { return Pending<ModType>{ASSERT_NOT_NULL(fut)}; }

  ASSERT(fut == nullptr);

  fut = &internal::pending_module_futures.emplace_back(
      std::async(std::launch::async,
                 [canonical_src, mod(&iter->second.second)]() -> BasicModule * {
                   // TODO error messages.
                   ASSIGN_OR(return nullptr,  //
                                    frontend::FileSource file_src,
                                    frontend::FileSource::Make(*canonical_src));
                   *mod = std::make_unique<ModType>();
                   (*mod)->ProcessFromSource(&file_src);
                   return mod->get();
                 }));
  return Pending<ModType>{fut};
}

}  // namespace module

#endif  // ICARUS_MODULE_PENDING_H
