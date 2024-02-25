#ifndef ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H
#define ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H

#include "absl/container/flat_hash_map.h"
#include "ir/function_id.h"
#include "ir/function.h"

namespace ic {

// Contains an indexing of all constant pointers in or transitively depended on
// by the currently-being-compiled module.
struct GlobalPointerRegistry {
  void Register(size_t, void *);
  size_t id(void *) const;
  void *pointer(size_t) const;

 private:
  absl::flat_hash_map<void *, size_t> ids_by_pointer_;
  absl::flat_hash_map<size_t, void *> pointers_by_id_;
};

inline GlobalPointerRegistry global_pointer_registry;

}  // namespace ic

#endif  // ICARUS_IR_GLOBAL_FUNCTION_REGISTRY_H
