#ifndef ICARUS_MODULE_SHARED_CONTEXT_H
#define ICARUS_MODULE_SHARED_CONTEXT_H

#include <concepts>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "base/flyweight_map.h"
#include "ir/value/fn.h"
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
 private:
  using void_fn_ptr = void (*)();

 public:
  explicit SharedContext(
      std::unique_ptr<BuiltinModule> m,
      base::any_invocable<ir::Inst(ir::InstructionProto const &)>
          instruction_deserializer)
      : table_(std::move(m)),
        instruction_deserializer_(std::move(instruction_deserializer)) {}
  // Given a name and a function type, returns the associated foreign function,
  // possibly declaring a new one if none already exists.
  ir::Fn ForeignFunction(std::string &&name, type::Function const *f) {
    auto [iter, inserted] =
        foreign_fn_map_.try_emplace(std::pair(std::move(name), f));
    return ir::Fn(ir::ModuleId::Foreign(),
                  ir::LocalFnId(std::distance(foreign_fn_map_.begin(), iter)));
  }
  ir::Fn ForeignFunction(std::string_view name, type::Function const *f) {
    return ForeignFunction(std::string(name), f);
  }

  Module::FunctionInformation Function(ir::Fn f) const {
    ASSERT(f.module() != ir::ModuleId::Foreign());
    return module_table().module(f.module())->Function(f.local());
  }

  auto &foreign_function_map() { return foreign_fn_map_; }
  auto const &foreign_function_map() const { return foreign_fn_map_; }

  void_fn_ptr ForeignFunctionPointer(ir::Fn f) const;
  type::Function const *ForeignFunctionType(ir::Fn f) const;

  ModuleTable &module_table() { return table_; }
  ModuleTable const &module_table() const { return table_; }

  absl::FunctionRef<ir::Inst(ir::InstructionProto const &)>
  instruction_deserializer() const {
    return instruction_deserializer_;
  }

 private:
  ModuleTable table_;
  base::flyweight_map<std::pair<std::string, type::Function const *>,
                      void (*)()>
      foreign_fn_map_;
  base::any_invocable<ir::Inst(ir::InstructionProto const &)>
      instruction_deserializer_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SHARED_CONTEXT_H
