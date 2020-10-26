#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "absl/functional/function_ref.h"
#include "base/global.h"
#include "base/meta.h"
#include "type/type.h"

namespace type {

struct Primitive : public LegacyType {
 public:
  enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  };

  TYPE_FNS(Primitive);
  constexpr Primitive(BasicType pt)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 1,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        type_(pt) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  template <typename Fn>
  auto Apply(Fn &&fn) const {
    return ApplyImpl<uint8_t, uint16_t, uint32_t, uint64_t, int8_t, int16_t,
                     int32_t, int64_t, float, double, bool,
                     Type /* TODO: Other primitives */>(std::forward<Fn>(fn));
  }

  base::MetaValue meta() const {
    return Apply([&]<typename T>()->base::MetaValue { return base::meta<T>; });
  }

  Completeness completeness() const override { return Completeness::Complete; }

  bool is_big() const override { return false; }
  bool is_integral() const;

  BasicType type_;

 private:
  template <typename... Ts, typename Fn>
  decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>())
  ApplyImpl(Fn &&fn) const;
};

namespace internal {

inline base::Global kPrimitiveArray = std::array{
    Primitive(Primitive::BasicType::Nat8),
    Primitive(Primitive::BasicType::Nat16),
    Primitive(Primitive::BasicType::Nat32),
    Primitive(Primitive::BasicType::Nat64),
    Primitive(Primitive::BasicType::Int8),
    Primitive(Primitive::BasicType::Int16),
    Primitive(Primitive::BasicType::Int32),
    Primitive(Primitive::BasicType::Int64),
    Primitive(Primitive::BasicType::Float32),
    Primitive(Primitive::BasicType::Float64),
    Primitive(Primitive::BasicType::Bool),
    Primitive(Primitive::BasicType::Type_),
    Primitive(Primitive::BasicType::NullPtr),
    Primitive(Primitive::BasicType::EmptyArray),
    Primitive(Primitive::BasicType::Scope),
    Primitive(Primitive::BasicType::Block),
    Primitive(Primitive::BasicType::Module),
    Primitive(Primitive::BasicType::ByteView),
    Primitive(Primitive::BasicType::Label),
};

}  // namespace internal

template <typename... Ts, typename Fn>
decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>())
Primitive::ApplyImpl(Fn &&fn) const {
  using return_type =
      decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>());
  // Because primitive types are unique, we can compare the address to
  // `kPrimitiveArray->data()` and use the offset to index into a collection of
  // function of our own creation.
  int index = static_cast<int>(this - internal::kPrimitiveArray->data());
  return std::array{absl::FunctionRef<return_type()>([&] {
    return std::forward<Fn>(fn).template operator()<Ts>();
  })...}[index]();
}

inline Type Nat8       = &(*internal::kPrimitiveArray)[0];
inline Type Nat16      = &(*internal::kPrimitiveArray)[1];
inline Type Nat32      = &(*internal::kPrimitiveArray)[2];
inline Type Nat64      = &(*internal::kPrimitiveArray)[3];
inline Type Int8       = &(*internal::kPrimitiveArray)[4];
inline Type Int16      = &(*internal::kPrimitiveArray)[5];
inline Type Int32      = &(*internal::kPrimitiveArray)[6];
inline Type Int64      = &(*internal::kPrimitiveArray)[7];
inline Type Float32    = &(*internal::kPrimitiveArray)[8];
inline Type Float64    = &(*internal::kPrimitiveArray)[9];
inline Type Bool       = &(*internal::kPrimitiveArray)[10];
inline Type Type_      = &(*internal::kPrimitiveArray)[11];
inline Type NullPtr    = &(*internal::kPrimitiveArray)[12];
inline Type EmptyArray = &(*internal::kPrimitiveArray)[13];
inline Type Scope      = &(*internal::kPrimitiveArray)[14];
inline Type Block      = &(*internal::kPrimitiveArray)[15];
inline Type Module     = &(*internal::kPrimitiveArray)[16];
inline Type ByteView   = &(*internal::kPrimitiveArray)[17];
inline Type Label      = &(*internal::kPrimitiveArray)[18];

inline bool IsNumeric(Type t) {
  return t.get() >= Nat8.get() and t.get() <= Float64.get();
}
inline bool IsIntegral(Type t) {
  return t.get() >= Nat8.get() and t.get() <= Int64.get();
}
inline bool IsUnsignedNumeric(Type t) {
  return t.get() >= Nat8.get() and t.get() <= Nat64.get();
}
inline bool IsSignedNumeric(Type t) {
  return t.get() >= Int8.get() and t.get() <= Float64.get();
}
inline bool IsFloatingPoint(Type t) {
  return t.get() >= Float32.get() and t.get() <= Float64.get();
}

}  // namespace type

#endif  // ICARUS_TYPE_PRIMITIVE_H
