#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "absl/functional/function_ref.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/integer.h"
#include "ir/value/module_id.h"
#include "ir/value/scope.h"
#include "type/argument.h"
#include "type/system.h"
#include "type/type.h"

namespace type {

struct Primitive : LegacyType {
 public:
  enum class Kind : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  };

  constexpr Primitive(Kind pt)
      : LegacyType(IndexOf<Primitive>(),
                   LegacyType::Flags{.is_default_initializable = 1,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}),
        kind_(pt) {}

  Kind kind() const { return kind_; }

  template <typename Fn>
  auto Apply(Fn &&fn) const {
    return ApplyImpl<uint8_t, uint16_t, uint32_t, uint64_t, ir::Integer, int8_t,
                     int16_t, int32_t, int64_t, float, double, bool, ir::Char,
                     std::byte, Type, ir::ModuleId, ir::ScopeContext,
                     ir::UnboundScope, ir::ModuleId, ir::addr_t, Argument
                     /* TODO: Other primitives */>(std::forward<Fn>(fn));
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

  bool is_big() const override;

 private:
  Kind kind_;

  template <typename... Ts, typename Fn>
  decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>())
  ApplyImpl(Fn &&fn) const;
};

namespace internal_primitive {

inline constinit std::array kPrimitiveArray{
    Primitive(Primitive::Kind::U8),
    Primitive(Primitive::Kind::U16),
    Primitive(Primitive::Kind::U32),
    Primitive(Primitive::Kind::U64),
    Primitive(Primitive::Kind::Integer),
    Primitive(Primitive::Kind::I8),
    Primitive(Primitive::Kind::I16),
    Primitive(Primitive::Kind::I32),
    Primitive(Primitive::Kind::I64),
    Primitive(Primitive::Kind::F32),
    Primitive(Primitive::Kind::F64),
    Primitive(Primitive::Kind::Bool),
    Primitive(Primitive::Kind::Char),
    Primitive(Primitive::Kind::Byte),
    Primitive(Primitive::Kind::Type_),
    Primitive(Primitive::Kind::Module),
    Primitive(Primitive::Kind::ScopeContext),
    Primitive(Primitive::Kind::UnboundScope),
    Primitive(Primitive::Kind::CallingModule),
    Primitive(Primitive::Kind::NullPtr),
    Primitive(Primitive::Kind::Argument),
    Primitive(Primitive::Kind::EmptyArray),
    Primitive(Primitive::Kind::Label),
    Primitive(Primitive::Kind::Interface),
    Primitive(Primitive::Kind::Void),
};

inline bool GlobalTypeSystemInitializer = [] {
  for (auto const &p : kPrimitiveArray) { GlobalTypeSystem.insert(Type(&p)); }
  return true;
}();

}  // namespace internal_primitive

template <typename... Ts, typename Fn>
decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>())
Primitive::ApplyImpl(Fn &&fn) const {
  using return_type =
      decltype(std::declval<Fn>().template operator()<base::first_t<Ts...>>());
  // Because primitive types are unique, we can compare the address to
  // `kPrimitiveArray->data()` and use the offset to index into a collection of
  // function of our own creation.
  int index =
      static_cast<int>(this - internal_primitive::kPrimitiveArray.data());
  return std::array{absl::FunctionRef<return_type()>([&] {
    return std::forward<Fn>(fn).template operator()<Ts>();
  })...}[index]();
}

inline Type U8            = &internal_primitive::kPrimitiveArray[0];
inline Type U16           = &internal_primitive::kPrimitiveArray[1];
inline Type U32           = &internal_primitive::kPrimitiveArray[2];
inline Type U64           = &internal_primitive::kPrimitiveArray[3];
inline Type Integer       = &internal_primitive::kPrimitiveArray[4];
inline Type I8            = &internal_primitive::kPrimitiveArray[5];
inline Type I16           = &internal_primitive::kPrimitiveArray[6];
inline Type I32           = &internal_primitive::kPrimitiveArray[7];
inline Type I64           = &internal_primitive::kPrimitiveArray[8];
inline Type F32           = &internal_primitive::kPrimitiveArray[9];
inline Type F64           = &internal_primitive::kPrimitiveArray[10];
inline Type Bool          = &internal_primitive::kPrimitiveArray[11];
inline Type Char          = &internal_primitive::kPrimitiveArray[12];
inline Type Byte          = &internal_primitive::kPrimitiveArray[13];
inline Type Type_         = &internal_primitive::kPrimitiveArray[14];
inline Type Module        = &internal_primitive::kPrimitiveArray[15];
inline Type ScopeContext  = &internal_primitive::kPrimitiveArray[16];
inline Type UnboundScope  = &internal_primitive::kPrimitiveArray[17];
inline Type CallingModule = &internal_primitive::kPrimitiveArray[18];
inline Type NullPtr       = &internal_primitive::kPrimitiveArray[19];
inline Type Argument_     = &internal_primitive::kPrimitiveArray[20];
inline Type EmptyArray    = &internal_primitive::kPrimitiveArray[21];
inline Type Label         = &internal_primitive::kPrimitiveArray[22];
inline Type Interface     = &internal_primitive::kPrimitiveArray[23];
inline Type Void          = &internal_primitive::kPrimitiveArray[24];

inline Type MakePrimitive(Primitive::Kind k) {
  return &internal_primitive::kPrimitiveArray
      [static_cast<std::underlying_type_t<Primitive::Kind>>(k)];
}

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
