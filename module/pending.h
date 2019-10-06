#ifndef ICARUS_MODULE_PENDING_H
#define ICARUS_MODULE_PENDING_H

#include <filesystem>
#include <future>

#include "base/expected.h"
#include "frontend/source/source.h"

struct Module;

namespace module {

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
    std::filesystem::path const &src, Module const *requestor,
    std::unique_ptr<Module> (*fn)(frontend::Source *));
void AwaitAllModulesTransitively();

}  // namespace module

#endif  // ICARUS_MODULE_PENDING_H
