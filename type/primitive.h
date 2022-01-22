#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "absl/functional/function_ref.h"
#include "base/global.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/integer.h"
#include "ir/value/module_id.h"
#include "ir/value/scope.h"
#include "type/type.h"

namespace type {

struct Primitive : LegacyType {
 public:
  enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  };

  constexpr Primitive(BasicType pt)
      : LegacyType(IndexOf<Primitive>(),
                   LegacyType::Flags{.is_default_initializable = 1,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        type_(pt) {}

  template <typename Fn>
  auto Apply(Fn &&fn) const {
    return ApplyImpl<uint8_t, uint16_t, uint32_t, uint64_t, ir::Integer, int8_t,
                     int16_t, int32_t, int64_t, float, double, bool, ir::Char,
                     std::byte, Type, ir::ModuleId, ir::addr_t,
                     ir::ScopeContext,
                     ir::UnboundScope /* TODO: Other primitives */>(
        std::forward<Fn>(fn));
  }

  base::MetaValue meta() const {
    return Apply(
        [&]<typename T>() -> base::MetaValue { return base::meta<T>; });
  }

  Completeness completeness() const override { return Completeness::Complete; }

  bool EqualsValue(ir::CompleteResultRef const &lhs,
                   ir::CompleteResultRef const &rhs) const override;
  size_t HashValue(ir::CompleteResultRef const &value) const override;
  void ShowValue(std::ostream &os,
                 ir::CompleteResultRef const &value) const override;

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool is_big() const override { return false; }

  BasicType type_;

 private:
  template <typename... Ts, typename Fn>
  decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>())
  ApplyImpl(Fn &&fn) const;
};

namespace internal {

inline base::Global kPrimitiveArray = std::array{
    Primitive(Primitive::BasicType::U8),
    Primitive(Primitive::BasicType::U16),
    Primitive(Primitive::BasicType::U32),
    Primitive(Primitive::BasicType::U64),
    Primitive(Primitive::BasicType::Integer),
    Primitive(Primitive::BasicType::I8),
    Primitive(Primitive::BasicType::I16),
    Primitive(Primitive::BasicType::I32),
    Primitive(Primitive::BasicType::I64),
    Primitive(Primitive::BasicType::F32),
    Primitive(Primitive::BasicType::F64),
    Primitive(Primitive::BasicType::Bool),
    Primitive(Primitive::BasicType::Char),
    Primitive(Primitive::BasicType::Byte),
    Primitive(Primitive::BasicType::Type_),
    Primitive(Primitive::BasicType::Module),
    Primitive(Primitive::BasicType::ScopeContext),
    Primitive(Primitive::BasicType::UnboundScope),
    Primitive(Primitive::BasicType::NullPtr),
    Primitive(Primitive::BasicType::EmptyArray),
    Primitive(Primitive::BasicType::Label),
    Primitive(Primitive::BasicType::Interface),
    Primitive(Primitive::BasicType::Void),
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

inline Type U8           = &(*internal::kPrimitiveArray)[0];
inline Type U16          = &(*internal::kPrimitiveArray)[1];
inline Type U32          = &(*internal::kPrimitiveArray)[2];
inline Type U64          = &(*internal::kPrimitiveArray)[3];
inline Type Integer      = &(*internal::kPrimitiveArray)[4];
inline Type I8           = &(*internal::kPrimitiveArray)[5];
inline Type I16          = &(*internal::kPrimitiveArray)[6];
inline Type I32          = &(*internal::kPrimitiveArray)[7];
inline Type I64          = &(*internal::kPrimitiveArray)[8];
inline Type F32          = &(*internal::kPrimitiveArray)[9];
inline Type F64          = &(*internal::kPrimitiveArray)[10];
inline Type Bool         = &(*internal::kPrimitiveArray)[11];
inline Type Char         = &(*internal::kPrimitiveArray)[12];
inline Type Byte         = &(*internal::kPrimitiveArray)[13];
inline Type Type_        = &(*internal::kPrimitiveArray)[14];
inline Type Module       = &(*internal::kPrimitiveArray)[15];
inline Type ScopeContext = &(*internal::kPrimitiveArray)[16];
inline Type UnboundScope = &(*internal::kPrimitiveArray)[17];
inline Type NullPtr      = &(*internal::kPrimitiveArray)[18];
inline Type EmptyArray   = &(*internal::kPrimitiveArray)[19];
inline Type Label        = &(*internal::kPrimitiveArray)[20];
inline Type Interface    = &(*internal::kPrimitiveArray)[21];
inline Type Void         = &(*internal::kPrimitiveArray)[22];

inline bool IsNumeric(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &U8.as<Primitive>() and p <= &F64.as<Primitive>();
}
inline bool IsIntegral(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &U8.as<Primitive>() and p <= &I64.as<Primitive>();
}
inline bool IsUnsignedNumeric(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &U8.as<Primitive>() and p <= &U64.as<Primitive>();
}
inline bool IsSignedNumeric(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &Integer.as<Primitive>() and p <= &F64.as<Primitive>();
}
inline bool IsFloatingPoint(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &F32.as<Primitive>() and p <= &F64.as<Primitive>();
}

inline Type PointerDifferenceType(core::Arch const &arch) {
  static std::array<Type, 8> kTypes{I8, I16, I32, I32, I64, I64, I64, I64};
  return kTypes[arch.pointer().bytes().value() - 1];
}

}  // namespace type

#endif  // ICARUS_TYPE_PRIMITIVE_H
