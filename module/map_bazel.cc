#include "module/map_bazel.h"

#include <optional>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/strings/str_split.h"
#include "base/file.h"

namespace module {

absl::StatusOr<ModuleMap> BazelModuleMap(
    std::string const& file_name, std::vector<std::string> lookup_paths) {
  if (file_name.empty()) { return ModuleMap({}, std::move(lookup_paths)); }

  std::vector<ModuleMap::Entry> entries;

  std::optional content = base::ReadFileToString(file_name);
  if (not content) {
    return absl::InvalidArgumentError(
        absl::StrCat("Failed to load module map '", file_name, "'."));
  }

  for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
    if (line.empty()) { continue; }

    size_t first_split = line.find("::");
    if (first_split == std::string_view::npos) { continue; }
    std::string_view import_name = line.substr(0, first_split);

    line.remove_prefix(first_split + 2);
    size_t second_split = line.find("::");
    if (second_split == std::string_view::npos) { continue; }
    std::string_view label = line.substr(0, second_split);

    line.remove_prefix(second_split + 2);
    std::string_view file_name = line;

    entries.push_back({.label       = std::string(label),
                       .file_name   = std::string(file_name),
                       .import_name = std::string(import_name)});
  }

  return ModuleMap(std::move(entries), std::move(lookup_paths));
}

}  // namespace module
