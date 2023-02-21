#ifndef ICARUS_TOOLCHAIN_BAZEL_H
#define ICARUS_TOOLCHAIN_BAZEL_H

#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/functional/any_invocable.h"
#include "base/file.h"
#include "module/module_name.h"
#include "serialization/module_map.h"

namespace toolchain {

struct BazelSpecification {
  absl::flat_hash_map<module::ModuleName, serialization::UniqueModuleId> names;
  std::vector<std::pair<serialization::UniqueModuleId, std::string>> paths;
};

std::optional<BazelSpecification> BazelModuleMap(std::string const& file_name);

// TODO: This has nothing to do with bazel.
absl::AnyInvocable<serialization::UniqueModuleId(module::ModuleName const&)
                       const>
BazelNameResolver(
    absl::flat_hash_map<module::ModuleName, serialization::UniqueModuleId> map);

}  // namespace toolchain

#endif  // ICARUS_TOOLCHAIN_BAZEL_H
