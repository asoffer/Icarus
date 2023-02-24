#include "module/global_function_map.h"

namespace module {

void GlobalFunctionMap::insert_function(
    void const* f, serialization::ModuleIndex module_index,
    serialization::FunctionIndex function_index) {
  functions_.try_emplace(f, module_index, function_index);
  reverse_functions_.try_emplace(std::pair(module_index, function_index), f);
}

std::pair<serialization::ModuleIndex, serialization::FunctionIndex>
GlobalFunctionMap::find(void const* f) {
  if (auto iter = functions_.find(f); iter == functions_.end()) {
    return std::pair(serialization::ModuleIndex::Invalid(),
                     serialization::FunctionIndex::Invalid());
  } else {
    return iter->second;
  }
}

void const* GlobalFunctionMap::find(serialization::ModuleIndex m,
                                    serialization::FunctionIndex f) {
  auto iter = reverse_functions_.find(std::pair(m, f));
  if (iter == reverse_functions_.end()) { return nullptr; }
  return iter->second;
}

}  // namespace module
