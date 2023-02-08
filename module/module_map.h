#ifndef ICARUS_MODULE_MODULE_MAP_H
#define ICARUS_MODULE_MODULE_MAP_H

#include <span>
#include <string>
#include <utility>
#include <vector>

#include "module/module.h"
#include "nth/container/flyweight_map.h"
#include "serialization/module_index.h"
#include "serialization/module_map.h"

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

// Represents a mapping from the three forms of identification for modules (a
// unique identifier for the module, the name specified in `import` expressions,
// and the path to the file on disk."
struct ModuleMap {
  ModuleMap() { emplace(serialization::UniqueModuleId("")); }
  virtual ~ModuleMap() {}

 protected:
  struct IdLookupResult;
  IdLookupResult find(serialization::ModuleIndex index) const;

  struct IdLookupResult {
    IdLookupResult() = default;

    operator bool() const { return ptr_; }

    serialization::UniqueModuleId const &id() const { return ptr_->first; }

   private:
    friend IdLookupResult ModuleMap::find(serialization::ModuleIndex) const;

    explicit IdLookupResult(
        std::pair<serialization::UniqueModuleId const, Module> const *ptr)
        : ptr_(ptr) {}
    std::pair<serialization::UniqueModuleId const, Module> const *ptr_ =
        nullptr;
  };

 public:
  virtual IdLookupResult id(ModuleName const &name) const = 0;

  Module &primary() { return ids_.from_index(0).second; }
  Module const &primary() const { return ids_.from_index(0).second; }

  serialization::ModuleIndex TryLoad(ModuleName const &name) const;

  Module &emplace(serialization::UniqueModuleId const &id);

  serialization::UniqueModuleId const &operator[](
      serialization::ModuleIndex index) const;

  serialization::ModuleIndex index(
      serialization::UniqueModuleId const &id) const;

 private:
  nth::flyweight_map<serialization::UniqueModuleId, Module> ids_;
};

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_MAP_H
