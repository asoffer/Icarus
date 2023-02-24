#ifndef ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
#define ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "serialization/function_index.h"
#include "serialization/module_index.h"
#include "serialization/unique_module_id.h"

namespace module {

struct GlobalFunctionMap {
  void insert_function(void const* f, serialization::ModuleIndex module_index,
                       serialization::FunctionIndex function_index);

  std::pair<serialization::ModuleIndex, serialization::FunctionIndex> find(
      void const* f);

  void const* find(serialization::ModuleIndex m,
                   serialization::FunctionIndex f);

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

#endif  // ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
