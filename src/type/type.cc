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

static base::guarded<base::map<base::vector<Type const *>,
                               base::map<base::vector<Type const *>, Function>>>
    funcs_;
const Function *Func(base::vector<Type const *> in,
                     base::vector<Type const *> out) {
  // TODO if void is unit in some way we shouldn't do this.
  auto f = Function(in, out);
  return &(*funcs_.lock())[std::move(in)]
              .emplace(std::move(out), std::move(f))
              .first->second;
}

Type const *Void() { return Tup({}); }

bool Type::is_big() const {
  return is<Array>() || is<Struct>() || is<Variant>() || is<Tuple>();
}

Type const *Generic = new GenericFunction;

ir::Val Tuple::PrepareArgument(Type const *t, ir::Val const &val,
                               Context *ctx) const {
  UNREACHABLE();
}

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
