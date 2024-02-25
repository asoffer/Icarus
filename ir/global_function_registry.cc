#include "ir/global_function_registry.h"

#include "nth/debug/debug.h"

namespace ic {

void GlobalPointerRegistry::Register(size_t id, void *ptr) {
  [[maybe_unused]] bool ptr_inserted = ids_by_pointer_.emplace(ptr, id).second;
  NTH_REQUIRE(ptr_inserted);

  [[maybe_unused]] bool id_inserted = pointers_by_id_.emplace(id, ptr).second;
  NTH_REQUIRE(id_inserted);
}

size_t GlobalPointerRegistry::id(void *ptr) const {
  auto iter = ids_by_pointer_.find(ptr);
  NTH_REQUIRE(iter != ids_by_pointer_.end())
      .Log<"Failed to find pointer {}">(ptr);
  return iter->second;
}

void *GlobalPointerRegistry::pointer(size_t id) const {
  auto iter = pointers_by_id_.find(id);
  NTH_REQUIRE(iter != pointers_by_id_.end())
      .Log<"Failed to find pointer by id {}">(id);
  return iter->second;
}

}  // namespace ic
