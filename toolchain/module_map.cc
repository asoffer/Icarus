#include "toolchain/module_map.h"

#include <cstddef>
#include <fstream>
#include <string>
#include <string_view>

#include "absl/strings/str_split.h"
#include "common/file.h"
#include "common/resources.h"
#include "ir/deserialize.h"
#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/string_reader.h"

namespace ic {

std::optional<std::vector<Module>> PopulateModuleMap(
    nth::file_path const& module_map_file) {
  std::optional content = ReadFileToString(module_map_file);
  if (not content) { return std::nullopt; }
  std::vector<Module> dependent_modules;
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
    NTH_REQUIRE(dependent_modules.size() + 1 == id.value());

    std::optional serialized_module_content = ReadFileToString(module_map_file);
    if (not serialized_module_content) { return std::nullopt; }
    ModuleDeserializer<nth::io::string_reader> deserializer(
        *serialized_module_content);
    if (not nth::io::deserialize(deserializer,
                                 dependent_modules.emplace_back())) {
      return std::nullopt;
    }
  }

  return dependent_modules;
}

}  // namespace ic
