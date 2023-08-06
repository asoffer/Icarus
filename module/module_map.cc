#include "module/module_map.h"

#include <fstream>

#include "absl/strings/str_split.h"
#include "base/file.h"
#include "module/module.h"
#include "nth/debug/debug.h"
#include "serialization/module.pb.h"

namespace module {

ModuleMap::ModuleMap(absl::flat_hash_map<ModuleName, UniqueId> names)
    : name_resolver_(std::move(names)) {}

std::optional<ModuleMap> ModuleMap::Load(nth::file_path const &path) {
  std::optional content = base::ReadFileToString(path);
  if (not content) { return std::nullopt; }

  if (content->empty()) { return ModuleMap(); }

  absl::flat_hash_map<ModuleName, UniqueId> names;

  std::vector<std::pair<UniqueId, std::string>> paths;
  module::UniqueId id;
  size_t index = 0;
  for (std::string_view line : absl::StrSplit(*content, absl::ByChar('\n'))) {
    switch (index) {
      case 0: {
        id    = module::UniqueId(line);
        index = 1;
      } break;
      case 1: {
        names.emplace(ModuleName(line), id);
        index = 2;
      } break;
      case 2: {
        paths.emplace_back(id, line);
        index = 0;
      } break;
    }
  }
  // TODO: Produce a proper diagnostic. We cannot rely on module maps being
  // properly formatted if we do not control how they were produced.
  NTH_ASSERT(index == 0);

  std::optional<ModuleMap> map(ModuleMap(std::move(names)));

  std::vector<std::pair<serialization::Module, Module *>> modules;
  map->imported_modules_.reserve(paths.size());
  for (auto const &[id, path] : paths) {
    std::ifstream stream(path);

    if (not stream.is_open()) {
      // TODO: Produce a proper diagnostic.
      NTH_LOG((v.always), "Failed to open {} ({}).") <<= {id.value(), path};
      return std::nullopt;
    }

    auto &[proto, mptr] = modules.emplace_back();
    if (not proto.ParseFromIstream(&stream)) {
      // TODO: Produce a proper diagnostic.
      NTH_LOG((v.always), "Failed to parse {} ({}).") <<= {id.value(), path};
      return std::nullopt;
    }

    mptr =
        map->imported_modules_.emplace_back(std::make_unique<Module>(id)).get();

    auto &gmm = map->global_module_map_;
    auto index = serialization::ModuleIndex(map->imported_modules_.size());
    gmm.insert(serialization::ModuleIndex::Self(), index, id);
    if (not GlobalModuleMap::Deserialize(index, proto.module_map(), gmm)) {
      // TODO: Produce a proper diagnostic.
      NTH_LOG((v.always), "Failed to load module {} ({}).") <<=
          {id.value(), path};
      return std::nullopt;
    }
  }

  // size_t i = 0;
  // for (auto const &[proto, mptr] : modules) {
  //   serialization::ModuleIndex index(i);
  //   if (not Module::DeserializeModuleInto(
  //           proto, resources.modules(), index, map->imported_modules_[i],
  //           resources.primary_module().type_system(),
  //           resources.unique_type_table(), resources.module_map(),
  //           resources.function_map(), resources.opaque_map())) {
  //     // TODO: Log an error.
  //     NTH_LOG((v.always), "Failed to deserialize module.");
  //     return std::nullopt;
  //   }
  //   ++i;
  // }

  return map;
}

}  // namespace module
