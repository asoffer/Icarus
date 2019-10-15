#include "type/pointer.h"

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "base/guarded.h"
#include "type/function.h"

namespace type {

static base::guarded<
    absl::flat_hash_map<Type const *, std::unique_ptr<Pointer const>>>
    pointers_;
Pointer const *Ptr(Type const *t) {
  auto handle = pointers_.lock();
  auto &p     = (*handle)[t];
  if (not p) { p = std::make_unique<Pointer>(t); }
  return p.get();
}

static base::guarded<
    absl::flat_hash_map<Type const *, std::unique_ptr<BufferPointer const>>>
    buffer_pointers_;
BufferPointer const *BufPtr(Type const *t) {
  auto handle = buffer_pointers_.lock();
  auto &p     = (*handle)[t];
  if (not p) { p = std::make_unique<BufferPointer>(t); }
  return p.get();
}

void static WriteStr(char const *ptr_str, Pointer const *ptr,
                     std::string *result) {
  bool needs_paren = ptr->pointee->is<Function>();
  result->append(ptr_str);
  if (needs_paren) { result->append("("); }
  ptr->pointee->WriteTo(result);
  if (needs_paren) { result->append(")"); }
}

void BufferPointer::WriteTo(std::string *r) const { WriteStr("[*]", this, r); }
void Pointer::WriteTo(std::string *r) const { WriteStr("*", this, r); }

core::Bytes Pointer::bytes(core::Arch const &a) const { return a.ptr_bytes; }

core::Alignment Pointer::alignment(core::Arch const &a) const {
  return a.ptr_alignment;
}

}  // namespace type
