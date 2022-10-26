#ifndef ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H
#define ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H

#include <functional>
#include <string>
#include <utility>

#include "base/flyweight_map.h"
#include "core/type_system/type_system.h"
#include "ir/value/fn.h"
#include "semantic_analysis/instruction_set.h"

namespace semantic_analysis {

struct ForeignFunctionMap {
  explicit ForeignFunctionMap(TypeSystem &type_system)
      : type_system_(type_system) {}

  auto begin() { return foreign_functions_.begin(); }
  auto end() { return foreign_functions_.end(); }
  auto begin() const { return foreign_functions_.begin(); }
  auto end() const { return foreign_functions_.end(); }
  auto cbegin() const { return foreign_functions_.begin(); }
  auto cend() const { return foreign_functions_.end(); }

  std::pair<ir::Fn, IrFunction const *> ForeignFunction(std::string name,
                                                        core::Type t);
  IrFunction const *ForeignFunction(ir::LocalFnId id) const;
  std::type_identity_t<void (*)()> ForeignFunctionPointer(
      ir::LocalFnId id) const;

 private:
  base::flyweight_map<std::pair<std::string, core::Type>,
                      std::pair<IrFunction, void (*)()>>
      foreign_functions_;
  TypeSystem &type_system_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_FOREIGN_FUNCTION_MAP_H
