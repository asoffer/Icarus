#ifndef ICARUS_MODULE_MODULE_MAP_H
#define ICARUS_MODULE_MODULE_MAP_H

#include <optional>

#include "module/global_module_map.h"
#include "module/module.h"
#include "module/module_name.h"
#include "module/name_resolver.h"
#include "module/unique_id.h"
#include "nth/io/file_path.h"

namespace module {

// A `ModuleMap` holds all (transitively) imported modules and provides access
// based on any of a number of naming/identification schemes.
//
// There are several ways a module can be named. Each module must be assigned a
// `UniqueId`, a string-like identifier which is unique amongst all modules
// linked into the same binary. The build system is responsible for defining
// such an identifier. Each module also corresponds to a single source file, for
// which we use `nth::file_path` as the representating type. The build system is
// responsible for making the association between a `UniqueId` and the
// corresponding source file. Lastly, modules are imported not via their source
// file, but by a `ModuleName`, another string-like identifier to be spelled in
// source. For example, the input/output module in the standard library may be
// imported via `io ::= import "std.io"`. Here the identifier "std.io" is the
// module name. The format of this string is implementation defined, though
// often corresponds to some portion of the path at which the source file is
// defined with directory separators replaced by '.' characters. That is, in
// this example, there may not actually be a file named "std.io", "std/io.ic",
// or anything similar. Once again, the build system is responsible for defining
// the mechanism by which modules are named.
struct ModuleMap {
  // Constructs an empty module-map.
  explicit ModuleMap() : name_resolver_({}) {}

  // Loads the module map from the file named by `path` returning the
  // constructed `ModuleMap` if the it was successfully loaded, and
  // `std::nullopt` otherwise.
  static std::optional<ModuleMap> Load(nth::file_path const& path);

  NameResolver const& name_resolver() const { return name_resolver_; }

  GlobalModuleMap& global_module_map() { return global_module_map_; }
  GlobalModuleMap const& global_module_map() const {
    return global_module_map_;
  }

 private:
  explicit ModuleMap(absl::flat_hash_map<ModuleName, UniqueId> names);

  NameResolver name_resolver_;
  std::vector<std::unique_ptr<Module>> imported_modules_;

  // TODO: Clean up this class.
  GlobalModuleMap global_module_map_;
};

}  // namespace module
#endif  // ICARUS_MODULE_MODULE_MAP_H
