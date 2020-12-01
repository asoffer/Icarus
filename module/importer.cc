#include "module/importer.h"

#include <cstdio>
#include <string>
#include <string_view>
#include <vector>

#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "frontend/source/file_name.h"

namespace module {
namespace {

bool FileExists(std::string const& path) {
  if (std::FILE* f = std::fopen(path.c_str(), "r")) {
    std::fclose(f);
    return true;
  }
  return false;
}

}  // namespace

frontend::CanonicalFileName ResolveModulePath(
    frontend::CanonicalFileName const& module_path,
    std::vector<std::string> const& lookup_paths) {
  // Respect absolute paths.
  if (absl::StartsWith(module_path.name(), "/")) { return module_path; }
  // Check for the module relative to the given lookup paths.
  for (std::string_view base_path : lookup_paths) {
    std::string path = absl::StrCat(base_path, "/", module_path.name());
    if (FileExists(path)) {
      return frontend::CanonicalFileName::Make(frontend::FileName(path));
    }
  }
  // Fall back to using the given path as-is, relative to $PWD.
  return module_path;
}
}  // namespace module
