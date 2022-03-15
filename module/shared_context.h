#ifndef ICARUS_MODULE_SHARED_CONTEXT_H
#define ICARUS_MODULE_SHARED_CONTEXT_H

#include <concepts>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "base/flyweight_map.h"
#include "ir/value/foreign_fn.h"
#include "module/builtin.h"
#include "module/module.h"
#include "module/table.h"
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
  explicit SharedContext(std::unique_ptr<BuiltinModule> m)
      : table_(std::move(m)) {}
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

  ModuleTable &module_table() { return table_; }
  ModuleTable const &module_table() const { return table_; }

 private:
  ModuleTable table_;
  base::flyweight_map<std::pair<std::string, type::Function const*>, void (*)()>
      foreign_fn_map_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SHARED_CONTEXT_H
