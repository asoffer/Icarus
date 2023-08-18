#include "module/global_function_map.h"

namespace module {

void GlobalFunctionMap::insert_function(void const* f, UniqueId module_id,
                                        LocalFnId function_id) {
  functions_.try_emplace(f, module_id, function_id);
  reverse_functions_.try_emplace(std::pair(module_id, function_id), f);
}

std::pair<UniqueId, LocalFnId> GlobalFunctionMap::find(void const* f) {
  if (auto iter = functions_.find(f); iter == functions_.end()) {
    return std::pair(UniqueId::Invalid(), LocalFnId::Invalid());
  } else {
    return iter->second;
  }
}

void const* GlobalFunctionMap::find(UniqueId m, LocalFnId f) {
  auto iter = reverse_functions_.find(std::pair(m, f));
  if (iter == reverse_functions_.end()) { return nullptr; }
  return iter->second;
}

}  // namespace module
