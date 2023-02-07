#ifndef ICARUS_MODULE_BAZEL_MODULE_MAP_H
#define ICARUS_MODULE_BAZEL_MODULE_MAP_H

#include <memory>

#include "base/file.h"
#include "module/specified_module_map.h"

namespace module {

std::unique_ptr<SpecifiedModuleMap> BazelModuleMap(
    std::string const& file_name);

}  // namespace module

#endif  // ICARUS_MODULE_BAZEL_MODULE_MAP_H
