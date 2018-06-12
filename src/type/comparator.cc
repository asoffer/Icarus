#include "all.h"

namespace type {
// TODO arrays are tricky because they may contain structs and so just using the
// result of this function is... maybe not what you intended.
Cmp Array::Comparator() const { return data_type->Comparator(); }

Cmp Primitive::Comparator() const {
  return (type_ == PrimType::Int || type_ == PrimType::Real) ? Cmp::Order
                                                             : Cmp::Equality;
}

Cmp Pointer::Comparator() const { return Cmp::Equality; }
Cmp Function::Comparator() const { NOT_YET(this); }
Cmp Enum::Comparator() const { return Cmp::Equality; }
Cmp Flags::Comparator() const { return Cmp::Order; }

Cmp Variant::Comparator() const {
  using cmp_t = std::underlying_type_t<Cmp>;
  auto cmp = static_cast<cmp_t>(Cmp::Equality);
  for (const Type *t : variants_) {
    cmp = std::min(cmp, static_cast<cmp_t>(t->Comparator()));
  }
  return static_cast<Cmp>(cmp);
}

Cmp Scope::Comparator() const { return Cmp::None; }
Cmp Struct::Comparator() const { return Cmp::None; }
Cmp CharBuffer::Comparator() const { return Cmp::Order; }

} // namespace type
