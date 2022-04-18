#ifndef ICARUS_MODULE_MAP_BAZEL_H
#define ICARUS_MODULE_MAP_BAZEL_H

#include <string>
#include <vector>

#include "absl/status/statusor.h"
#include "module/map.h"

namespace module {

absl::StatusOr<ModuleMap> BazelModuleMap(std::string const& file_name,
                                         std::vector<std::string> lookup_paths);

}  // namespace module

#endif  // ICARUS_MODULE_MAP_BAZEL_H
