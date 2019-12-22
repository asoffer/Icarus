#ifndef ICARUS_MODULE_PENDING_H
#define ICARUS_MODULE_PENDING_H

#include <future>
#include <utility>

#include "base/expected.h"
#include "base/macros.h"
#include "frontend/source/file_name.h"
#include "frontend/source/source.h"
#include "module.h"

namespace module {

template <typename ModType>
struct Pending;

template <typename ModType>
base::expected<Pending<ModType>> ImportModule(
    frontend::FileName const &src, BasicModule const *requestor,
    std::unique_ptr<ModType> (*fn)(frontend::Source *));

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
  friend base::expected<Pending<BasicModule>> ImportModuleImpl(
      frontend::FileName const &src, std::unique_ptr<BasicModule> (*creator)());

  friend base::expected<Pending<ModType>> ImportModule<ModType>(
      frontend::FileName const &src, BasicModule const *requestor,
      std::unique_ptr<ModType> (*fn)(frontend::Source *));

  uintptr_t data_ = 0;

  Pending() = default;
  explicit Pending(ModType *mod) : data_(reinterpret_cast<uintptr_t>(mod)) {}
  explicit Pending(std::shared_future<BasicModule *> *mod)
      : data_(reinterpret_cast<uintptr_t>(mod) | 0x01) {}
};

void AwaitAllModulesTransitively();

base::expected<Pending<BasicModule>> ImportModuleImpl(
    frontend::FileName const &src, std::unique_ptr<BasicModule> (*creator)());

template <typename ModType>
base::expected<Pending<ModType>> ImportModule(frontend::FileName const &src) {
  auto import = ImportModuleImpl(src, []() -> std::unique_ptr<BasicModule> {
    return std::make_unique<ModType>();
  });
  ASSIGN_OR(return _.error(), auto p , import);
  return *reinterpret_cast<Pending<ModType>*>(&p);
}

}  // namespace module

#endif  // ICARUS_MODULE_PENDING_H
