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

bool Pointer::EqualsValue(ir::CompleteResultRef const &lhs,
                          ir::CompleteResultRef const &rhs) const {
  base::untyped_buffer_view lhs_view = lhs.raw();
  base::untyped_buffer_view rhs_view = rhs.raw();
  if (lhs_view.size() != rhs_view.size()) { return false; }
  return std::memcmp(lhs_view.data(), lhs_view.data(), lhs_view.size()) == 0;
}

size_t Pointer::HashValue(ir::CompleteResultRef const &value) const {
  return absl::Hash<absl::Span<std::byte const>>()(
      absl::Span<std::byte const>(value.raw().data(), value.raw().size()));
}

void Pointer::ShowValue(std::ostream &os,
                        ir::CompleteResultRef const &value) const {
  absl::Format(&os, "%p", value.get<ir::addr_t>());
}

}  // namespace type
