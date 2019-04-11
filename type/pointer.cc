#include "type/pointer.h"

#include "absl/container/flat_hash_set.h"
#include "absl/container/flat_hash_map.h"
#include "base/guarded.h"
#include "ir/cmd.h"
#include "type/function.h"

namespace type {

static base::guarded<absl::flat_hash_map<Type const *, Pointer const *>>
    pointers_;
Pointer const *Ptr(Type const *t) {
  return pointers_.lock()->emplace(t, new Pointer(t)).first->second;
}

static base::guarded<absl::flat_hash_map<Type const *, BufferPointer const *>>
    buffer_pointers_;
BufferPointer const *BufPtr(Type const *t) {
  return buffer_pointers_.lock()
      ->emplace(t, new BufferPointer(t))
      .first->second;
}

void Pointer::EmitCopyAssign(Type const *from_type, ir::Results const &from,
                         ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  if (this == from_type) {
    ir::Store(from.get<ir::Addr>(0), to);
  } else if (from_type == NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE();
  }
}

void Pointer::EmitMoveAssign(Type const *from_type, ir::Results const &from,
                         ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  if (this == from_type) {
    ir::Store(from.get<ir::Addr>(0), to);
  } else if (from_type == NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE(this);
  }
}

void Pointer::defining_modules(
    absl::flat_hash_set<::Module const *> *modules) const {
  pointee->defining_modules(modules);
}

void Pointer::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::Addr::Null(), id_reg);
}

void Pointer::EmitRepr(ir::Results const &val, Context *ctx) const {
  ir::Print(val.get<ir::Addr>(0));
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

ir::Results Pointer::PrepareArgument(Type const *from, ir::Results const &val,
                                     Context *ctx) const {
  ASSERT(from == this);
  return val;
}

core::Bytes Pointer::bytes(core::Arch const &a) const {
  return a.ptr_bytes;
}

core::Alignment Pointer::alignment(core::Arch const &a) const {
  return a.ptr_alignment;
}

Cmp Pointer::Comparator() const { return Cmp::Equality; }

bool Pointer::ReinterpretAs(Type const *t) const {
  auto *to_ptr = t->if_as<Pointer>();
  if (!to_ptr) { return false; }
  if (to_ptr->is<BufferPointer>()) { return false; }
  return pointee->ReinterpretAs(to_ptr->pointee);
}

bool BufferPointer::ReinterpretAs(Type const *t) const {
  if (auto *to_ptr = t->if_as<Pointer>()) {
    return pointee->ReinterpretAs(to_ptr->pointee);
  } else {
    return false;
  }
}

}  // namespace type
