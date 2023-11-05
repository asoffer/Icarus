#ifndef ICARUS_TOOLCHAIN_MODULE_MAP_H
#define ICARUS_TOOLCHAIN_MODULE_MAP_H

#include <optional>
#include <vector>

#include "ir/module.pb.h"
#include "nth/io/file_path.h"

namespace ic {

std::optional<std::vector<ModuleProto>> PopulateModuleMap(
    nth::file_path const& module_map_file);

}  // namespace ic

#endif // ICARUS_TOOLCHAIN_MODULE_MAP_H
