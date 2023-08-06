#ifndef ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
#define ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "module/function_id.h"
#include "module/unique_id.h"

namespace module {

struct GlobalFunctionMap {
  void insert_function(void const* f, UniqueId module_id,
                       LocalFnId function_id);

  std::pair<UniqueId, LocalFnId> find(void const* f);

  void const* find(UniqueId id, LocalFnId f);

 private:
  absl::flat_hash_map<void const*, std::pair<UniqueId, LocalFnId>> functions_;
  absl::flat_hash_map<std::pair<UniqueId, LocalFnId>, void const*>
      reverse_functions_;
};

}  // namespace module

#endif  // ICARUS_MODULE_GLOBAL_FUNCTION_MAP_H
