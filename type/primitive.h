#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

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

#define PRIMITIVE_MACRO(EnumName, name)                                        \
  namespace internal {                                                         \
  inline Primitive const EnumName##_Primitive{BasicType::EnumName};            \
  }                                                                            \
  inline constexpr Type const *EnumName = &internal::EnumName##_Primitive;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO

// TODO lay these out adjacent in memory so the tests can be faster.
inline bool IsIntegral(Type const *t) {
  return t == Int8 or t == Int16 or t == Int32 or t == Int64 or t == Nat8 or
         t == Nat16 or t == Nat32 or t == Nat64;
}

inline bool IsFloatingPoint(Type const *t) {
  return t == Float32 or t == Float64;
}

inline bool IsNumeric(Type const *t) {
  return IsIntegral(t) or IsFloatingPoint(t);
}

}  // namespace type

#endif  // ICARUS_TYPE_PRIMITIVE_H
