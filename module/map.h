#ifndef ICARUS_MODULE_MAP_H
#define ICARUS_MODULE_MAP_H

#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_cat.h"
#include "base/debug.h"

namespace module {

// When compiling multiple Icarus modules, there are three descriptors for
// modules whose relations need to be tracked.
// * Label: Each module is given a name (called its "label") which is unique
//   amongst all modules being linked together.
// * File name: Each precompiled module is stored in a file. The "filename" is
//   an identifier through which we can access the precompiled module contents
//   on the compiling platform.
// * Import name: When importing a module via an `import` statement, this is the
//   string which identifies the module. This identifier may be dependent on the
//   compliation context. For example, this name may be a relative file path, or
//   dependent on compiler flags specific to the compilation of the current
//   module.
//
// Note that none of these descriptors need to overlap in any meaningful way,
// and only the label is required to uniquely identify a module (and only with
// respect to the modules being linked together).
struct ModuleMap {
  struct Entry {
    std::string label;
    std::string file_name;
    std::string import_name;
  };

  explicit ModuleMap(std::vector<Entry> entries,
                     std::vector<std::string> lookup_paths)
      : entries_(std::move(entries)), lookup_paths_(std::move(lookup_paths)) {
    for (auto const &entry : entries_) {
      by_label_.try_emplace(entry.label, &entry);
      by_import_name_.try_emplace(entry.import_name, &entry);
    }
  }

  Entry const *by_label(std::string_view label) const {
    auto iter = by_label_.find(label);
    ASSERT(iter != by_label_.end());
    return iter->second;
  }

  Entry const *by_import_name(std::string_view import_name) const {
    for (std::string_view base : lookup_paths_) {
      auto iter = by_import_name_.find(absl::StrCat(base, "/", import_name));
      if (iter != by_import_name_.end()) { return iter->second; }
    }

    auto iter = by_import_name_.find(import_name);
    if (iter != by_import_name_.end()) { return iter->second; }
    return nullptr;
  }

 private:
  std::vector<Entry> entries_;
  std::vector<std::string> lookup_paths_;

  absl::flat_hash_map<std::string_view, Entry const *> by_label_;
  absl::flat_hash_map<std::string_view, Entry const *> by_import_name_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MAP_H
