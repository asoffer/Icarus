#include "type/pointer.h"

#include "absl/container/flat_hash_map.h"
#include "base/global.h"

namespace type {

static base::Global<absl::flat_hash_map<Type, Pointer const *>> pointer_cache;

Pointer const *Ptr(Type t) {
  auto handle = pointer_cache.lock();
  auto &p     = (*handle)[t];
  if (not p) { p = new Pointer(t); }
  return p;
}

static base::Global<absl::flat_hash_map<Type, BufferPointer const *>>
    buffer_pointer_cache;

BufferPointer const *BufPtr(Type t) {
  auto handle = buffer_pointer_cache.lock();
  auto &p     = (*handle)[t];
  if (not p) { p = new BufferPointer(t); }
  return p;
}

void static WriteStr(char const *ptr_str, Pointer const *ptr,
                     std::string *result) {
  result->append(ptr_str);
  ptr->pointee().get()->WriteTo(result);
  result->append(")");
}

void BufferPointer::WriteTo(std::string *r) const { WriteStr("[*](", this, r); }
void Pointer::WriteTo(std::string *r) const { WriteStr("*(", this, r); }

core::Bytes Pointer::bytes(core::Arch const &a) const {
  return a.pointer().bytes();
}

core::Alignment Pointer::alignment(core::Arch const &a) const {
  return a.pointer().alignment();
}

}  // namespace type
