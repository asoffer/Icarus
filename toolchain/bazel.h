#ifndef ICARUS_TOOLCHAIN_BAZEL_H
#define ICARUS_TOOLCHAIN_BAZEL_H

#include <string>

#include "absl/container/flat_hash_map.h"
#include "absl/functional/any_invocable.h"
#include "base/file.h"
#include "module/module_name.h"
#include "module/unique_id.h"
#include "nth/io/file_path.h"

namespace toolchain {

struct BazelSpecification {
  absl::flat_hash_map<module::ModuleName, module::UniqueId> names;
  std::vector<std::pair<module::UniqueId, std::string>> paths;
};

std::optional<BazelSpecification> BazelModuleMap(
    nth::file_path const& file_name);

// TODO: This has nothing to do with bazel.
absl::AnyInvocable<module::UniqueId(module::ModuleName const&) const>
BazelNameResolver(
    absl::flat_hash_map<module::ModuleName, module::UniqueId> map);

}  // namespace toolchain

#endif  // ICARUS_TOOLCHAIN_BAZEL_H
