#include "function_table.h"

#include "nth/debug/debug.h"

namespace vm {

Function const& FunctionTable::function(module::LocalFnId index) const {
  NTH_ASSERT(index.value() < functions_.size());
  return functions_[index.value()];
}

module::LocalFnId FunctionTable::find(Function const* f) {
  if (auto iter = function_indices_.find(f); iter != function_indices_.end()) {
    return iter->second;
  } else {
    return module::LocalFnId::Invalid();
  }
}

std::pair<module::LocalFnId, Function*> FunctionTable::emplace(
    size_t parameters, size_t returns, module::UniqueId module_id) {
  module::LocalFnId index(functions_.size());
  auto& f = functions_.emplace_back(parameters, returns);
  function_indices_.emplace(&f, index);

  // Note: This is correct because we only call `emplace` on the
  // currently-being compiled function.

  function_map_.insert_function(&f, module_id, index);

  return std::pair(index, &f);
}

}  // namespace vm
