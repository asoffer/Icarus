#ifndef ICARUS_MODULE_GLOBAL_MODULE_MAP_H
#define ICARUS_MODULE_GLOBAL_MODULE_MAP_H

#include <string>
#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "module/unique_id.h"
#include "nth/container/flyweight_set.h"
#include "serialization/module_index.h"
#include "serialization/proto/module_map.pb.h"

namespace module {

// Represents a mapping of module indices as specified in one module to another.
// Each module labels its dependencies with an index type (namely,
// `serialization::ModuleIndex`), however, these labels are only unique within
// the context of the module currently being compiled. That is, the module
// corresponding to `ModuleIndex(7)` when compiling "foo.ic" might be entirely
// different than when compiling "bar.ic". Of course, we could use a globally
// unique label at all times, however getting such an identifier is problematic.
// If we wish to assign a numeric value that is unique, we need to either know
// ahead of time all modules being linked into a binary, or somehow provide
// stateful compilation in which module A may not depend on module B, but
// changes to B could still change the numeric value associated with A. Each of
// these is hostile to caching. An alternative is to provide a non-numeric
// identifier, like a string-based label. However looking up data based on these
// labels is more expensive than an integer-like label, and trickier to
// serialize in functions. To combat that, within a given module we associate a
// `ModuleIndex` to each value globally unique label. This index is only
// meaningful within the context of the module currently being compiled.
struct GlobalModuleMap {
  // Inserts `id` into the module map and associates it with `dep_index` when
  // read from `module_index`.
  void insert(serialization::ModuleIndex module_index,
              serialization::ModuleIndex dep_index, module::UniqueId const& id);

  // Given the module-specific index represented by `module_index` and
  // `dep_index`, returns the index relative to the currently being
  // compiled/interpreted module.
  serialization::ModuleIndex read(serialization::ModuleIndex module_index,
                                  serialization::ModuleIndex dep_index) const;

  // Returns the `serialization::ModuleIndex` associated with `id` relative to
  // `serialization::ModuleIndex::Self()` if one exists and
  // `serialization::ModuleIndex::Invalid()` otherwise.
  serialization::ModuleIndex index(module::UniqueId const& id) const;

  static void Serialize(GlobalModuleMap const& from,
                        serialization::proto::ModuleMap& to);
  static bool Deserialize(serialization::ModuleIndex self_index,
                          serialization::proto::ModuleMap const& from,
                          GlobalModuleMap& to);

 private:
  nth::flyweight_set<module::UniqueId> data_;
  absl::flat_hash_map<
      std::pair<serialization::ModuleIndex, serialization::ModuleIndex>,
      serialization::ModuleIndex>
      mapping_;
};

}  // namespace module

#endif  // ICARUS_MODULE_GLOBAL_MODULE_MAP_H
