#ifndef ICARUS_CORE_PENDING_MODULE_H
#define ICARUS_CORE_PENDING_MODULE_H

#include <filesystem>
#include <future>

#include "base/expected.h"

struct Module;

namespace core {

// A `PendingModule` represents a module that is currently being loaded but for
// which the data may not yet be available.
struct PendingModule {
 public:
  PendingModule() = default;
  explicit PendingModule(Module *mod)
      : data_(reinterpret_cast<uintptr_t>(mod)) {}
  explicit PendingModule(std::shared_future<Module *> *mod)
      : data_(reinterpret_cast<uintptr_t>(mod) | 0x01) {}

  // Returns the compiled module, possibly blocking if `get` is called before
  // the module has finished compiling.
  Module *get();

  bool valid() const { return data_ != 0; }

 private:
  uintptr_t data_ = 0;
};

base::expected<PendingModule> ImportModule(
    std::filesystem::path const &src, std::filesystem::path const &requestor,
    Module *(*fn)(Module *, std::filesystem::path const *));

void AwaitAllModulesTransitively();

}  // namespace core

#endif  // ICARUS_CORE_PENDING_MODULE_H
