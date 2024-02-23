#include "toolchain/module_map.h"

#include <cstddef>
#include <string>
#include <string_view>

#include "absl/strings/str_split.h"
#include "common/resources.h"
#include "ir/deserialize.h"
#include "jasmin/core/function_registry.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/reader/file.h"
#include "nth/io/reader/string.h"

namespace ic {

std::optional<DependentModules> PopulateModuleMap(
    nth::file_path const& module_map_file, SharedContext& context) {
  std::optional reader = nth::io::file_reader::try_open(module_map_file);
  if (not reader) { return std::nullopt; }

  std::string content(reader->size(), '\0');
  if (not reader->read(std::span<std::byte>(
          reinterpret_cast<std::byte*>(content.data()), content.size()))) {
    return std::nullopt;
  }

  DependentModules dependent_modules;
  for (std::string_view line : absl::StrSplit(content, absl::ByChar('\n'))) {
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

    std::optional serialized_module_reader =
        nth::io::file_reader::try_open(*path);
    if (not serialized_module_reader) { return std::nullopt; }

    std::string serialized_module_content(serialized_module_reader->size(), '\0');
    if (not serialized_module_reader->read(std::span<std::byte>(
            reinterpret_cast<std::byte*>(serialized_module_content.data()),
            serialized_module_content.size()))) {
      return std::nullopt;
    }

    ModuleDeserializer<nth::io::string_reader> deserializer(
        serialized_module_content, context);
    Result r = nth::io::deserialize(deserializer, dependent_modules.add(name));
    if (not r) { return std::nullopt; }
  }

  return dependent_modules;
}

}  // namespace ic
