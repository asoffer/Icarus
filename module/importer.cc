#include "module/importer.h"

#include <cstdio>
#include <cstdlib>
#include <string_view>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
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
    frontend::CanonicalFileName const& module_path) {
  // Respect absolute paths.
  if (absl::StartsWith(module_path.name(), "/")) { return module_path; }
  // Walk the entries in $ICARUS_MODULE_PATH to find a valid source file.
  if (char* const lookup_paths = std::getenv("ICARUS_MODULE_PATH")) {
    for (std::string_view base_path : absl::StrSplit(lookup_paths, ':')) {
      std::string path = absl::StrCat(base_path, "/", module_path.name());
      if (FileExists(path)) {
        return frontend::CanonicalFileName::Make(frontend::FileName(path));
      }
    }
  }
  // Fall back to using the given path as-is, relative to $PWD.
  return module_path;
}
}  // namespace module
