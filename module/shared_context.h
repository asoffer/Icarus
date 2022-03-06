#ifndef ICARUS_MODULE_SHARED_CONTEXT_H
#define ICARUS_MODULE_SHARED_CONTEXT_H

#include <concepts>
#include <string>
#include <string_view>
#include <utility>

#include "base/flyweight_map.h"
#include "ir/value/foreign_fn.h"
#include "module/module.h"
#include "type/function.h"

namespace module {

// Because compilation can be distributed across many invocations of the same
// binary, much of the data computed will not match up identically between
// modules when it should. Any value whose representation is relative to some
// global data will have this problem, because "global" is relative to the
// binary currently being executed. Foreign functions and types are two such
// examples: Foreign functions are represented as a pointer to data describing
// the foreign function. The `SharedContext` is a data structure that allows us
// to reconcile these disagreements.
struct SharedContext {
  // Given a name and a function type, returns the associated foreign function,
  // possibly declaring a new one if none already exists.
  ir::ForeignFn ForeignFunction(std::string &&name, type::Function const *f) {
    return ir::ForeignFn(
        foreign_fn_map_.try_emplace(std::pair(std::move(name), f)).first);
  }
  ir::ForeignFn ForeignFunction(std::string_view name,
                                type::Function const *f) {
    return ForeignFunction(std::string(name), f);
  }

  auto &foreign_function_map() { return foreign_fn_map_; }

  template <std::derived_from<Module> ModuleType, typename... Args>
  ModuleType &add_module(std::string id, Args &&... args) requires(
      std::constructible_from<ModuleType, std::string, Args...>) {
    auto iter = modules_.find(id);
    if (iter != modules_.end()) { return iter->second->as<ModuleType>(); }

    auto m = std::make_unique<ModuleType>(std::move(id),
                                          std::forward<Args>(args)...);

    std::string_view module_id = m->identifier();
    return modules_.emplace(module_id, std::move(m))
        .first->second->template as<ModuleType>();
  }

  Module const *module(std::string_view id) const {
    auto iter = modules_.find(id);
    if (iter == modules_.end()) { return nullptr; }
    return iter->second.get();
  }

 private:
  absl::flat_hash_map<std::string_view, std::unique_ptr<Module>> modules_;
  base::flyweight_map<std::pair<std::string, type::Function const*>, void (*)()>
      foreign_fn_map_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SHARED_CONTEXT_H
