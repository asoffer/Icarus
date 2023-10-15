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
  void Register(FunctionId id, IrFunction const *f) {
    [[maybe_unused]] bool ptr_inserted =
        functions_by_ptr_.emplace(f, id).second;
    NTH_REQUIRE(ptr_inserted);

    [[maybe_unused]] bool id_inserted = functions_by_id_.emplace(id, f).second;
    NTH_REQUIRE(id_inserted);
  }

  FunctionId id(IrFunction const *f) const {
    auto iter = functions_by_ptr_.find(f);
    NTH_REQUIRE(iter != functions_by_ptr_.end())
        .Log<"Failed to find function {}">(f);
    return iter->second;
  }

  IrFunction const &function(FunctionId id) const {
    auto iter = functions_by_id_.find(id);
    NTH_REQUIRE(iter != functions_by_id_.end())
        .Log<"Failed to find function by id {}">(id);
    return *iter->second;
  }

 private:
  absl::flat_hash_map<IrFunction const *, FunctionId> functions_by_ptr_;
  absl::flat_hash_map<FunctionId, IrFunction const *> functions_by_id_;
};

inline GlobalFunctionRegistry global_function_registry;

}  // namespace ic

#endif  // ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H
