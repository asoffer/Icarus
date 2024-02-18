#include "toolchain/module_map.h"

#include <cstddef>
#include <string>
#include <string_view>

#include "absl/strings/str_split.h"
#include "common/file.h"
#include "common/resources.h"
#include "ir/deserialize.h"
#include "nth/io/reader/string.h"
#include "nth/io/serialize/deserialize.h"

namespace ic {

std::optional<DependentModules> PopulateModuleMap(
    nth::file_path const& module_map_file) {
  std::optional content = ReadFileToString(module_map_file);
  if (not content) { return std::nullopt; }
  DependentModules dependent_modules;
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
    NTH_REQUIRE((v.harden), dependent_modules.count() == id.value());

    std::optional path = nth::file_path::try_construct(location);
    if (not path) { return std::nullopt; }
    std::optional serialized_module_content = ReadFileToString(*path);
    if (not serialized_module_content) { return std::nullopt; }
    ModuleDeserializer<nth::io::string_reader> deserializer(
        *serialized_module_content);
    if (not nth::io::deserialize(deserializer, dependent_modules.add(name))) {
      return std::nullopt;
    }
  }

  return dependent_modules;
}

}  // namespace ic
