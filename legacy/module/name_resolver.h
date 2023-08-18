#ifndef ICARUS_MODULE_NAME_RESOLVER_H
#define ICARUS_MODULE_NAME_RESOLVER_H

#include "absl/container/flat_hash_map.h"
#include "module/module_name.h"
#include "module/unique_id.h"

namespace module {

// Represents a mechanism by which one can convert from from a
// `module::ModuleName` to a `module::UniqueId`.
struct NameResolver {
  explicit NameResolver(absl::flat_hash_map<ModuleName, UniqueId> map)
      : map_(std::move(map)) {}

  UniqueId operator()(ModuleName const& name) const;

 private:
  absl::flat_hash_map<ModuleName, UniqueId> map_;
};

}  // namespace module

#endif  // ICARUS_MODULE_NAME_RESOLVER_H
