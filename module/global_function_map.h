#ifndef ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
#define ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "module/unique_id.h"
#include "serialization/function_index.h"

namespace module {

struct GlobalFunctionMap {
  void insert_function(void const* f, UniqueId module_id,
                       serialization::FunctionIndex function_index);

  std::pair<UniqueId, serialization::FunctionIndex> find(void const* f);

  void const* find(UniqueId id, serialization::FunctionIndex f);

 private:
  absl::flat_hash_map<void const*,
                      std::pair<UniqueId, serialization::FunctionIndex>>
      functions_;
  absl::flat_hash_map<std::pair<UniqueId, serialization::FunctionIndex>,
                      void const*>
      reverse_functions_;
};

}  // namespace module

#endif  // ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
