#ifndef ICARUS_SERIALIZATION_MODULE_MAP_H
#define ICARUS_SERIALIZATION_MODULE_MAP_H

#include "nth/container/flyweight_set.h"
#include "serialization/module_index.h"
#include "serialization/proto/module_map.pb.h"
#include "serialization/unique_module_id.h"

namespace serialization {

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
struct ModuleMap {
  // Returns the `ModuleIndex` associated with `id` if one exists and
  // `ModuleIndex::Invalid()` otherwise.
  ModuleIndex index(UniqueModuleId const& id) const;

  // Returns the `UniqueModuleId` associated with `index`.
  UniqueModuleId const& id(ModuleIndex index) const;

  std::pair<ModuleIndex, bool> insert(UniqueModuleId const& id);

  static void Serialize(ModuleMap const& from, proto::ModuleMap& to);
  static bool Deserialize(proto::ModuleMap const& from, ModuleMap& to);

 private:
  nth::flyweight_set<UniqueModuleId> data_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_MODULE_MAP_H
