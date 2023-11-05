#include "ir/global_function_registry.h"

#include "nth/debug/debug.h"

namespace ic {

void GlobalFunctionRegistry::Register(FunctionId id, IrFunction const *f) {
  [[maybe_unused]] bool ptr_inserted = functions_by_ptr_.emplace(f, id).second;
  NTH_REQUIRE(ptr_inserted);

  [[maybe_unused]] bool id_inserted = functions_by_id_.emplace(id, f).second;
  NTH_REQUIRE(id_inserted);
}

FunctionId GlobalFunctionRegistry::id(IrFunction const *f) const {
  auto iter = functions_by_ptr_.find(f);
  NTH_REQUIRE(iter != functions_by_ptr_.end())
      .Log<"Failed to find function {}">(f);
  return iter->second;
}

IrFunction const &GlobalFunctionRegistry::function(FunctionId id) const {
  auto iter = functions_by_id_.find(id);
  NTH_REQUIRE(iter != functions_by_id_.end())
      .Log<"Failed to find function by id {}">(id);
  return *iter->second;
}

}  // namespace ic
