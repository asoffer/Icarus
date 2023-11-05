#include "toolchain/module_map.h"

#include <cstddef>
#include <fstream>
#include <string_view>

#include "absl/strings/str_split.h"
#include "common/file.h"
#include "common/resources.h"

namespace ic {

std::optional<std::vector<ModuleProto>> PopulateModuleMap(
    nth::file_path const& module_map_file) {
  std::optional content = ReadFileToString(module_map_file);
  if (not content) {
    return std::nullopt; }
  std::vector<ModuleProto> dependent_module_protos;
  for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
    if (line.empty()) { continue; }
    size_t count = 0;
    std::string_view name, location;
    for (std::string_view chunk : absl::StrSplit(line, absl::ByChar('\t'))) {
      switch (count++) {
        case 0: name = chunk; break;
        case 1: location = chunk; break;
        default: return std::nullopt;
      }
    }
    auto id = resources.module_map.add(name);
    NTH_REQUIRE(dependent_module_protos.size() + 1 == id.value());

    std::ifstream in{std::string(location)};
    if (not in.is_open()) { NTH_UNIMPLEMENTED("{}") <<= {location}; }
    dependent_module_protos.emplace_back().ParseFromIstream(&in);
  }

  return dependent_module_protos;
}

}  // namespace ic
