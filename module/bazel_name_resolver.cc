#include "module/bazel_name_resolver.h"

#include <string>

#include "absl/strings/str_split.h"
#include "base/debug.h"

namespace module {

absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
BazelNameResolver(std::string const& file_name) {
  std::optional content = base::ReadFileToString(file_name);
  if (not content) { return nullptr; }

  absl::flat_hash_map<ModuleName, serialization::UniqueModuleId> specification;
  if (content->empty()) {
    return [](ModuleName const& name) -> serialization::UniqueModuleId {
      UNREACHABLE("No such module: ", name.name());
    };
  }

  serialization::UniqueModuleId id;
  size_t index = 0;
  for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
    switch (index) {
      case 0: {
        id = serialization::UniqueModuleId(line);
      } break;
      case 1: {
        specification.emplace(ModuleName(line), id);
      } break;
    }
    index = 1 - index;
  }
  ASSERT(index == 0);

  return [specification = std::move(specification)](ModuleName const& name) {
    auto iter = specification.find(name);
    ASSERT(iter != specification.end());
    return iter->second;
  };
}

}  // namespace module
