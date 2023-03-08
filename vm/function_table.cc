#include "function_table.h"

#include "base/debug.h"

namespace vm {

Function const& FunctionTable::function(
    serialization::FunctionIndex index) const {
  ASSERT(index.value() < functions_.size());
  return functions_[index.value()];
}

serialization::FunctionIndex FunctionTable::find(Function const* f) {
  if (auto iter = function_indices_.find(f); iter != function_indices_.end()) {
    return iter->second;
  } else {
    return serialization::FunctionIndex::Invalid();
  }
}

std::pair<serialization::FunctionIndex, Function*> FunctionTable::emplace(
    size_t parameters, size_t returns,
    serialization::ModuleIndex module_index) {
  serialization::FunctionIndex index(functions_.size());
  auto& f = functions_.emplace_back(parameters, returns);
  function_indices_.emplace(&f, index);

  // Note: This is correct because we only call `emplace` on the
  // currently-being compiled function.

  function_map_.insert_function(&f, module_index, index);

  return std::pair(index, &f);
}

}  // namespace vm