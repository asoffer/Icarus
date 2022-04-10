#ifndef ICARUS_MODULE_TABLE_H
#define ICARUS_MODULE_TABLE_H

#include <concepts>
#include <string>
#include <string_view>
#include <utility>

#include "ir/value/module_id.h"
#include "module/builtin.h"
#include "module/module.h"

namespace module {

// A data structure representing an associative mapping from either a numeric or
// unique string identifier for a module to that module. String identifiers are
// provided by and must be unique amongst all modules in the table. Numeric
// identifiers are assigned by the table itself.
struct ModuleTable {
  explicit ModuleTable(std::unique_ptr<BuiltinModule> m) {
    modules_.push_back(std::move(m));
    numeric_id_.emplace(modules_.back()->identifier(), ir::ModuleId::Builtin());
  }

  // Returns the number of modules contained in this ModuleTable.
  size_t size() const {
    ASSERT(modules_.size() == numeric_id_.size());
    return modules_.size();
  }

  // Adds a module of type `ModuleType` into the table constructed with the
  // given arguments if a module by the identifier `id` does not already exist
  // in the table. Otherwise, does nothing. In either case, after calling this
  // function, a module with the given `id` exists in the table and a pair is
  // returned consisting of the numeric module identifier and a reference to the
  // module by that identifier.
  //
  // If a module is with the given identifier already exists but does not have
  // type `ModuleType`, behavior is undefined.
  template <std::derived_from<Module> ModuleType, int &..., typename... Args>
  std::pair<ir::ModuleId, ModuleType *>
  add_module(std::string id, Args &&... args) requires(
      std::constructible_from<ModuleType, std::string, ir::ModuleId, Args...>) {
    if (auto [numeric_id, mod] = module(id); mod) {
      return std::pair<ir::ModuleId, ModuleType *>(numeric_id,
                                                   &mod->as<ModuleType>());
    }

    ir::ModuleId numeric_id(modules_.size());
    auto m = std::make_unique<ModuleType>(std::move(id), numeric_id,
                                          std::forward<Args>(args)...);
    std::string_view string_id = m->identifier();
    std::pair<ir::ModuleId, ModuleType *> result(numeric_id, m.get());
    modules_.push_back(std::move(m));
    numeric_id_.emplace(string_id, numeric_id);
    return result;
  }

  // Adds the module `m` to the module table, provided that no module is already
  // present with the same identifier `m.identifier()`.
  template <int &..., std::derived_from<Module> ModuleType>
  std::pair<ir::ModuleId, ModuleType *> add_module(ModuleType &&m) {
    std::string_view string_id = m.identifier();
    if (auto [numeric_id, mod] = module(string_id); mod) {
      return std::pair<ir::ModuleId, ModuleType *>(numeric_id,
                                                   &mod->as<ModuleType>());
    }

    ir::ModuleId numeric_id(modules_.size());
    auto mp = std::make_unique<ModuleType>(std::move(m));
    std::pair<ir::ModuleId, ModuleType *> result(numeric_id, mp.get());
    modules_.push_back(std::move(mp));
    numeric_id_.emplace(string_id, numeric_id);
    return result;
  }

  // If a module identified by `id` is present in the table, returns a pair
  // consisting of its numeric id and a pointer to that module. Otherwise,
  // returns a pair consisting of `ir::ModuleId::Invalid()` and a null pointer.
  std::pair<ir::ModuleId, Module *> module(std::string_view id) {
    if (auto iter = numeric_id_.find(id); iter == numeric_id_.end()) {
      return std::pair<ir::ModuleId, Module *>(ir::ModuleId::Invalid(),
                                               nullptr);
    } else {
      return std::pair<ir::ModuleId, Module *>(
          iter->second, modules_[iter->second.value()].get());
    }
  }

  // If a module identified by `id` is present in the table, returns a pair
  // consisting of its numeric id and a pointer to that module. Otherwise,
  // returns a pair consisting of `ir::ModuleId::Invalid()` and a null pointer.
  std::pair<ir::ModuleId, Module const *> module(std::string_view id) const {
    if (auto iter = numeric_id_.find(id); iter == numeric_id_.end()) {
      return std::pair<ir::ModuleId, Module *>(ir::ModuleId::Invalid(),
                                               nullptr);
    } else {
      return std::pair<ir::ModuleId, Module *>(
          iter->second, modules_[iter->second.value()].get());
    }
  }

  // If a module by the given `numeric_id` is present in the table, returns a
  // pointer to the corresponding module. Otherwise returns a null pointer.
  Module *module(ir::ModuleId numeric_id) {
    if (numeric_id.value() < modules_.size()) {
      return modules_[numeric_id.value()].get();
    }
    return nullptr;
  }
  // If a module by the given `numeric_id` is present in the table, returns a
  // pointer to the corresponding module. Otherwise returns a null pointer.
  Module const *module(ir::ModuleId numeric_id) const {
    if (numeric_id.value() < modules_.size()) {
      return modules_[numeric_id.value()].get();
    }
    return nullptr;
  }

  ir::ModuleId id(Module const *m) const {
    return module(m->identifier()).first;
  }

 private:
  std::vector<std::unique_ptr<Module>> modules_;
  absl::flat_hash_map<std::string_view, ir::ModuleId> numeric_id_;
};

}  // namespace module

#endif  // ICARUS_MODULE_TABLE_H
