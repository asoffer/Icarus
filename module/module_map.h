#ifndef ICARUS_MODULE_MODULE_MAP_H
#define ICARUS_MODULE_MODULE_MAP_H

#include <span>
#include <string>
#include <utility>
#include <vector>

#include "base/debug.h"
#include "nth/container/flyweight_map.h"

namespace module {

// Represents the name of a module as specifiable in an `import` expression.
struct ModuleName {
  explicit ModuleName(std::string &&name) : name_(std::move(name)) {}
  explicit ModuleName(std::string_view name = "") : name_(name) {}
  explicit ModuleName(char const *name) : name_(name) {}

  std::string_view name() const { return name_; }

  friend bool operator==(ModuleName const &, ModuleName const &) = default;
  template <typename H>
  friend H AbslHashValue(H h, ModuleName const &id) {
    return H::combine(std::move(h), id.name_);
  }

 private:
  std::string name_;
};

// Represents an identifier for the module which is unique amongst all modules
// linked into the same binary.
struct UniqueModuleId {
  explicit UniqueModuleId(std::string value) : value_(std::move(value)) {}
  explicit UniqueModuleId(std::string_view value = "") : value_(value) {}
  explicit UniqueModuleId(char const *value) : value_(value) {}

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

// Represents a path to a file on disk.
struct FilePath {
  explicit FilePath(std::string path = "") : path_(std::move(path)) {}

  std::string_view path() const { return path_; }

 private:
  std::string path_;
};

// An integer index into the module map
struct ModuleIndex {
  explicit ModuleIndex(uint64_t index) : index_(index) {}

  uint64_t value() const { return index_; }

 private:
  uint64_t index_;
};

// Represents a mapping from the three forms of identification for modules (a
// unique identifier for the module, the name specified in `import` expressions,
// and the path to the file on disk."
struct ModuleMap {
  virtual ~ModuleMap() {}

 protected:
  struct IdLookupResult;
  IdLookupResult find(ModuleIndex index) const;

  struct IdLookupResult {
    IdLookupResult() = default;

    operator bool() const { return ptr_; }

    UniqueModuleId const &id() const { return ptr_->first; }
    std::span<FilePath const> paths() const { return ptr_->second; }

   private:
    friend IdLookupResult ModuleMap::find(ModuleIndex) const;

    explicit IdLookupResult(
        std::pair<UniqueModuleId const, std::vector<FilePath>> const *ptr)
        : ptr_(ptr) {}
    std::pair<UniqueModuleId const, std::vector<FilePath>> const *ptr_ =
        nullptr;
  };

 public:
  virtual IdLookupResult id(ModuleName const &name) const = 0;

  void emplace(UniqueModuleId const &id, auto paths_begin, auto paths_end) {
    ids_.try_emplace(id, std::vector<FilePath>(paths_begin, paths_end));
  }

  UniqueModuleId const &operator[](ModuleIndex index) const {
    ASSERT(index.value() < ids_.size());
    return ids_.from_index(index.value()).first;
  }

  ModuleIndex index(UniqueModuleId const &id) const {
    return ModuleIndex(ids_.index(id));
  }

  std::span<FilePath const> paths(UniqueModuleId const &id);
  std::span<FilePath const> paths(ModuleIndex index);

 private:
  nth::flyweight_map<UniqueModuleId, std::vector<FilePath>> ids_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_MAP_H
