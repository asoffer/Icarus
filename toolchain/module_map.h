#ifndef ICARUS_TOOLCHAIN_MODULE_MAP_H
#define ICARUS_TOOLCHAIN_MODULE_MAP_H

#include <optional>

#include "ir/dependent_modules.h"
#include "ir/module.h"
#include "nth/io/file_path.h"

namespace ic {

std::optional<DependentModules> PopulateModuleMap(
    nth::file_path const& module_map_file);

}  // namespace ic

#endif // ICARUS_TOOLCHAIN_MODULE_MAP_H
