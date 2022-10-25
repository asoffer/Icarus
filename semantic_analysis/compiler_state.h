#ifndef ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H
#define ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H

#include <utility>

#include "core/parameters.h"
#include "core/type_system/type_system.h"
#include "ir/value/fn.h"
#include "module/module.h"
#include "semantic_analysis/foreign_function_map.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct CompilerState {
  explicit CompilerState(module::Module &module) : module_(module) {}

  TypeSystem const &type_system() const { return module_.type_system(); }
  TypeSystem &type_system() { return module_.type_system(); }

  module::Module const &module() const { return module_; }
  module::Module &module() { return module_; }

  ForeignFunctionMap const &foreign_function_map() const {
    return foreign_function_map_;
  }

  ForeignFunctionMap &foreign_function_map() { return foreign_function_map_; }

  IrFunction const *function(ir::Fn fn_id) const {
    if (fn_id.module() == ir::ModuleId::Foreign()) {
      return foreign_function_map_.ForeignFunction(fn_id.local());
    } else if (fn_id.module() == ir::ModuleId(0)) {
      size_t index = fn_id.local().value();
      ASSERT(index < functions_.size());
      return &functions_[index];
    } else {
      NOT_YET();
    }
  }

  std::pair<ir::Fn, IrFunction *> create_function(size_t parameters,
                                                  size_t returns) {
    ir::Fn fn(ir::ModuleId(0), ir::LocalFnId(functions_.size()));
    return std::pair(fn, &functions_.emplace_back(parameters, returns));
  }

 private:
  module::Module &module_;
  ForeignFunctionMap foreign_function_map_{module_.type_system()};
  std::deque<IrFunction> functions_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H
