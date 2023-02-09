#ifndef ICARUS_SERIALIZATION_MODULE_MAP_H
#define ICARUS_SERIALIZATION_MODULE_MAP_H

#include <string>
#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "nth/container/flyweight_set.h"
#include "serialization/module_index.h"

namespace serialization {

// Represents an identifier for the module which is unique amongst all modules
// linked into the same binary.
struct UniqueModuleId {
  explicit UniqueModuleId(std::string &&value) : value_(std::move(value)) {}
  explicit UniqueModuleId(std::string_view value = "") : value_(value) {}
  explicit UniqueModuleId(char const *value) : value_(value) {}

  // Valid modules must only use printable characters.
  static UniqueModuleId Invalid() {
    return UniqueModuleId(std::string("\0", 1));
  }

  std::string_view value() const { return value_; }

  friend bool operator==(UniqueModuleId const &,
                         UniqueModuleId const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, UniqueModuleId const &id) {
    return H::combine(std::move(h), id.value_);
  }

 private:
  std::string value_;
};

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
  // Inserts `id` into the module map and associates it with `dep_index` when
  // read from `module_index`.
  void insert(ModuleIndex module_index, ModuleIndex dep_index,
              UniqueModuleId const &id);

  // Given the module-specific index represented by `module_index` and `index`,
  // returns a pair consisting of the globally unique index and a view of the
  // associated value.
  std::pair<ModuleIndex, UniqueModuleId const &> read(
      ModuleIndex module_index, ModuleIndex dep_index) const;

  // Returns the `ModuleIndex` associated with `id` if one exists and
  // `ModuleIndex::Invalid()` otherwise.
  ModuleIndex get(UniqueModuleId const & id) const;

 private:
  nth::flyweight_set<UniqueModuleId> data_;
  absl::flat_hash_map<std::pair<ModuleIndex, ModuleIndex>, ModuleIndex>
      mapping_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_MODULE_MAP_H
