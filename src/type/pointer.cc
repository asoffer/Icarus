#include "type/pointer.h"

#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "ir/cmd.h"
#include "type/function.h"

namespace type {

static base::guarded<base::unordered_map<Type const *, Pointer const>>
    pointers_;
Pointer const *Ptr(Type const *t) {
  return &pointers_.lock()->emplace(t, Pointer(t)).first->second;
}

static base::guarded<base::unordered_map<Type const *, BufferPointer const>>
    buffer_pointers_;
BufferPointer const *BufPtr(Type const *t) {
  return &buffer_pointers_.lock()->emplace(t, BufferPointer(t)).first->second;
}

void Pointer::EmitCopyAssign(Type const *from_type, ir::Val const &from,
                         ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  if (this == from_type) {
    ir::Store(from.reg_or<ir::Addr>(), to);
  } else if (from_type == NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE();
  }
}

void Pointer::EmitMoveAssign(Type const *from_type, ir::Val const &from,
                         ir::RegisterOr<ir::Addr> to, Context *ctx) const {
  if (this == from_type) {
    ir::Store(from.reg_or<ir::Addr>(), to);
  } else if (from_type == NullPtr) {
    ir::Store(ir::Addr::Null(), to);
  } else {
    UNREACHABLE(this);
  }
}

void Pointer::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  pointee->defining_modules(modules);
}

void Pointer::EmitInit(ir::Register id_reg, Context *ctx) const {
  ir::Store(ir::Addr::Null(), id_reg);
}

void Pointer::EmitRepr(ir::Val const &val, Context *ctx) const {
  ir::Print(val.reg_or<ir::Addr>());
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

}  // namespace type
