#ifndef ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H
#define ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H

#include "absl/container/flat_hash_map.h"
#include "ir/function_id.h"
#include "ir/function.h"

namespace ic {

// Contains an indexing of all callable functions in or transitively depended on
// by the currently-being-compiled module. Notably, module initializers are not
// indexed because they cannot be called explicitly.
struct GlobalFunctionRegistry {
  void Register(FunctionId id, IrFunction const *f);
  FunctionId id(IrFunction const *f) const;
  IrFunction const &function(FunctionId id) const;

 private:
  absl::flat_hash_map<IrFunction const *, FunctionId> functions_by_ptr_;
  absl::flat_hash_map<FunctionId, IrFunction const *> functions_by_id_;
};

inline GlobalFunctionRegistry global_function_registry;

}  // namespace ic

#endif  // ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H
