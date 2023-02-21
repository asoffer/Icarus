#include "toolchain/bazel.h"

#include <string>

#include "absl/strings/str_split.h"
#include "base/debug.h"

namespace toolchain {

std::optional<BazelSpecification> BazelModuleMap(std::string const& file_name) {
  std::optional content = base::ReadFileToString(file_name);

  std::optional<BazelSpecification> specification;
  if (content) {
    if (specification.emplace(); content->empty()) { return specification; }
    serialization::UniqueModuleId id;
    size_t index = 0;
    for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
      switch (index) {
        case 0: {
          id    = serialization::UniqueModuleId(line);
          index = 1;
        } break;
        case 1: {
          specification->names.emplace(module::ModuleName(line), id);
          index = 2;
        } break;
        case 2: {
          specification->paths.emplace_back(id, line);
          index = 0;
        } break;
      }
    }
    ASSERT(index == 0);
  }
  return specification;
}

absl::AnyInvocable<serialization::UniqueModuleId(module::ModuleName const&)
                       const>
BazelNameResolver(
    absl::flat_hash_map<module::ModuleName, serialization::UniqueModuleId>
        map) {
  return [specification = std::move(map)](module::ModuleName const& name) {
    auto iter = specification.find(name);
    ASSERT(iter != specification.end());
    return iter->second;
  };
}

}  // namespace toolchain
