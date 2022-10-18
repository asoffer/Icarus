#ifndef ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H
#define ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H

#include <utility>

#include "core/parameters.h"
#include "core/type_system/type_system.h"
#include "ir/value/fn.h"
#include "semantic_analysis/foreign_function_map.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

struct CompilerState {
  TypeSystem const &type_system() const { return type_system_; }
  TypeSystem &type_system() { return type_system_; }

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
  TypeSystem type_system_;
  ForeignFunctionMap foreign_function_map_{type_system_};
  std::deque<IrFunction> functions_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_COMPILER_STATE_H
