#ifndef ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H
#define ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H

#include <functional>
#include <string>
#include <utility>

#include "core/type_system/type_system.h"
#include "data_types/fn.h"
#include "nth/container/flyweight_map.h"
#include "semantic_analysis/instruction_set.h"

namespace semantic_analysis {

struct ForeignFunctionMap {
  auto begin() { return foreign_functions_.begin(); }
  auto end() { return foreign_functions_.end(); }
  auto begin() const { return foreign_functions_.begin(); }
  auto end() const { return foreign_functions_.end(); }
  auto cbegin() const { return foreign_functions_.begin(); }
  auto cend() const { return foreign_functions_.end(); }

  std::pair<data_types::Fn, IrFunction const *> ForeignFunction(
      std::string name, core::FunctionType t);
  IrFunction const *ForeignFunction(data_types::LocalFnId id) const;
  std::type_identity_t<void (*)()> ForeignFunctionPointer(
      data_types::LocalFnId id) const;

 private:
  nth::flyweight_map<std::pair<std::string, core::FunctionType>,
                     std::pair<IrFunction, void (*)()>>
      foreign_functions_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H
