#include "type/all.h"

namespace type {
// TODO arrays are tricky because they may contain structs and so just using the
// result of this function is... maybe not what you intended.
Cmp Array::Comparator() const { return data_type->Comparator(); }

Cmp Primitive::Comparator() const {
  // TODO is this right for floating-point type?
  return (type_ == PrimType::Int8 || type_ == PrimType::Int16 ||
          type_ == PrimType::Int32 || type_ == PrimType::Int64 ||
          type_ == PrimType::Nat8 || type_ == PrimType::Nat16 ||
          type_ == PrimType::Nat32 || type_ == PrimType::Nat64 ||
          type_ == PrimType::Float32 || type_ == PrimType::Float64)
             ? Cmp::Order
             : Cmp::Equality;
}

Cmp Pointer::Comparator() const { return Cmp::Equality; }
Cmp Function::Comparator() const { return Cmp::None; }
Cmp Enum::Comparator() const { return Cmp::Equality; }
Cmp Flags::Comparator() const { return Cmp::Order; }
Cmp GenericFunction::Comparator() const { Cmp::None; }

Cmp Variant::Comparator() const {
  using cmp_t = std::underlying_type_t<Cmp>;
  auto cmp    = static_cast<cmp_t>(Cmp::Equality);
  for (const Type *t : variants_) {
    cmp = std::min(cmp, static_cast<cmp_t>(t->Comparator()));
  }
  return static_cast<Cmp>(cmp);
}

Cmp Struct::Comparator() const { return Cmp::None; }
Cmp CharBuffer::Comparator() const { return Cmp::Order; }

}  // namespace type
