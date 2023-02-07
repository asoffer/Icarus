#include "module/bazel_module_map.h"

#include <span>
#include <string>

#include "absl/strings/str_split.h"

namespace module {

std::unique_ptr<SpecifiedModuleMap> BazelModuleMap(
    std::string const& file_name) {
  std::unique_ptr<SpecifiedModuleMap> result;
  std::optional content = base::ReadFileToString(file_name);
  if (content) {
    result = std::make_unique<SpecifiedModuleMap>();

    UniqueModuleId id;
    ModuleName name;
    FilePath path;
    size_t index = 0;
    for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
      switch (index) {
        case 0: id = UniqueModuleId(std::string(line)); break;
        case 1: name = ModuleName(std::string(line)); break;
        case 2: path = FilePath(std::string(line)); break;
      }
      if (index == 2) {
        index = 0;
        std::span span(&path, 1);
        result->emplace(id, span.begin(), span.end());
        result->specify(name, id);

      } else {
        ++index;
      }
    }
    ASSERT(index == 0);
  }
  return result;
}

}  // namespace module
