#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "base/global.h"
#include "type/basic_type.h"
#include "type/type.h"

namespace type {

struct Primitive : public Type {
 public:
  TYPE_FNS(Primitive);
  constexpr Primitive(BasicType pt)
      : Type(Type::Flags{.is_default_initializable = 1,
                         .is_copyable              = 1,
                         .is_movable               = 1,
                         .has_destructor           = 0}),
        type_(pt) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  bool is_big() const override { return false; }
  bool is_integral() const;

  BasicType type_;
};

namespace internal {

inline base::Global kPrimitiveArray = std::array{
    Primitive(BasicType::Nat8),    Primitive(BasicType::Nat16),
    Primitive(BasicType::Nat32),   Primitive(BasicType::Nat64),
    Primitive(BasicType::Int8),    Primitive(BasicType::Int16),
    Primitive(BasicType::Int32),   Primitive(BasicType::Int64),
    Primitive(BasicType::Float32), Primitive(BasicType::Float64),
    Primitive(BasicType::Bool),    Primitive(BasicType::Type_),
    Primitive(BasicType::NullPtr), Primitive(BasicType::EmptyArray),
    Primitive(BasicType::Scope),   Primitive(BasicType::Block),
    Primitive(BasicType::Module),  Primitive(BasicType::ByteView),
    Primitive(BasicType::Label),
};

}  // namespace internal

inline Type const *Nat8       = &(*internal::kPrimitiveArray)[0];
inline Type const *Nat16      = &(*internal::kPrimitiveArray)[1];
inline Type const *Nat32      = &(*internal::kPrimitiveArray)[2];
inline Type const *Nat64      = &(*internal::kPrimitiveArray)[3];
inline Type const *Int8       = &(*internal::kPrimitiveArray)[4];
inline Type const *Int16      = &(*internal::kPrimitiveArray)[5];
inline Type const *Int32      = &(*internal::kPrimitiveArray)[6];
inline Type const *Int64      = &(*internal::kPrimitiveArray)[7];
inline Type const *Float32    = &(*internal::kPrimitiveArray)[8];
inline Type const *Float64    = &(*internal::kPrimitiveArray)[9];
inline Type const *Bool       = &(*internal::kPrimitiveArray)[10];
inline Type const *Type_      = &(*internal::kPrimitiveArray)[11];
inline Type const *NullPtr    = &(*internal::kPrimitiveArray)[12];
inline Type const *EmptyArray = &(*internal::kPrimitiveArray)[13];
inline Type const *Scope      = &(*internal::kPrimitiveArray)[14];
inline Type const *Block      = &(*internal::kPrimitiveArray)[15];
inline Type const *Module     = &(*internal::kPrimitiveArray)[16];
inline Type const *ByteView   = &(*internal::kPrimitiveArray)[17];
inline Type const *Label      = &(*internal::kPrimitiveArray)[18];

inline bool IsNumeric(Type const *t) { return t >= Nat8 and t <= Float64; }
inline bool IsIntegral(Type const *t) { return t >= Nat8 and t <= Int64; }
inline bool IsUnsignedNumeric(Type const *t) {
  return t >= Nat8 and t <= Nat64;
}
inline bool IsSignedNumeric(Type const *t) {
  return t >= Int8 and t <= Float64;
}
inline bool IsFloatingPoint(Type const *t) {
  return t >= Float32 and t <= Float64;
}

}  // namespace type

#endif  // ICARUS_TYPE_PRIMITIVE_H
