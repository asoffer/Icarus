#include "module/bazel_module_map.h"

#include <string>

#include "absl/strings/str_split.h"

namespace module {

std::unique_ptr<SpecifiedModuleMap> BazelModuleMap(
    std::string const& file_name) {
  std::unique_ptr<SpecifiedModuleMap> result;
  if (std::optional content = base::ReadFileToString(file_name)) {
    result = std::make_unique<SpecifiedModuleMap>();
    if (content->empty()) { return result; }
    UniqueModuleId id;
    size_t index = 0;
    for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
      switch (index) {
        case 0: {
          id = UniqueModuleId(std::string(line));
        } break;
        case 1: {
          result->identify(ModuleName(line), id);
        } break;
      }
      index = 1 - index;
    }
    ASSERT(index == 0);
  }
  return result;
}

}  // namespace module
