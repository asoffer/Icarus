#ifndef ICARUS_SEMANTIC_ANALYSIS_MODULE_BUILTIN_H
#define ICARUS_SEMANTIC_ANALYSIS_MODULE_BUILTIN_H

#include <functional>
#include <string>
#include <utility>

#include "base/flyweight_map.h"
#include "core/type_system/type_system.h"
#include "ir/value/fn.h"

namespace semantic_analysis {

struct BuiltinModule {
  std::pair<ir::Fn, void (*)()> ForeignFunction(std::string name, core::Type t);
  std::type_identity_t<void (*)()> ForeignFunction(ir::LocalFnId id);

 private:
  base::flyweight_map<std::pair<std::string, core::Type>, void (*)()> foreign_functions_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_MODULE_BUILTIN_H
