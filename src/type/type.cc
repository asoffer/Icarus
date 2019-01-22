#include "type/all.h"

#include "architecture.h"
#include "ast/declaration.h"
#include "ast/struct_literal.h"
#include "base/container/map.h"
#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "context.h"
#include "ir/arguments.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"

namespace type {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  Type const *EnumName = new Primitive(PrimType::EnumName);
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

static base::guarded<base::unordered_map<Type const *, Pointer const>>
    pointers_;
Pointer const *Ptr(Type const *t) {
  return &pointers_.lock()->emplace(t, Pointer(t)).first->second;
}

static base::guarded<base::unordered_map<Type const *, BufferPointer const >>
    buffer_pointers_;
BufferPointer const *BufPtr(Type const *t) {
  return &buffer_pointers_.lock()->emplace(t, BufferPointer(t)).first->second;
}

Type const *Void() { return Tup({}); }

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
}

Type const *Generic = new GenericFunction;

void Pointer::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  pointee->defining_modules(modules);
}

void Primitive::defining_modules(
    std::unordered_set<::Module const *> *modules) const {}

void Variant::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  for (auto *v : variants_) { v->defining_modules(modules); }
}

void Enum::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

void Flags::defining_modules(
    std::unordered_set<::Module const *> *modules) const {
  NOT_YET();
}

}  // namespace type
