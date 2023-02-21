#ifndef ICARUS_MODULE_FUNCTION_MAP_H
#define ICARUS_MODULE_FUNCTION_MAP_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "serialization/function_index.h"
#include "serialization/module_index.h"

namespace module {

struct FunctionMap {
  void insert_function(void const* f, serialization::ModuleIndex module_index,
                       serialization::FunctionIndex function_index) {
    functions_.try_emplace(f, module_index, function_index);
    reverse_functions_.try_emplace(std::pair(module_index, function_index), f);
  }

  std::pair<serialization::ModuleIndex, serialization::FunctionIndex> find(
      void const* f) {
    if (auto iter = functions_.find(f); iter == functions_.end()) {
      return std::pair(serialization::ModuleIndex::Invalid(),
                       serialization::FunctionIndex::Invalid());
    } else {
      return iter->second;
    }
  }

  void const* find(serialization::ModuleIndex m,
                   serialization::FunctionIndex f) {
    auto iter = reverse_functions_.find(std::pair(m, f));
    if (iter == reverse_functions_.end()) { return nullptr; }
    return iter->second;
  }

 private:
  absl::flat_hash_map<void const*, std::pair<serialization::ModuleIndex,
                                             serialization::FunctionIndex>>
      functions_;
  absl::flat_hash_map<
      std::pair<serialization::ModuleIndex, serialization::FunctionIndex>,
      void const*>
      reverse_functions_;
};

}  // namespace module

#endif  // ICARUS_MODULE_FUNCTION_MAP_H
