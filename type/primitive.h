#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "absl/functional/function_ref.h"
#include "base/global.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/char.h"
#include "ir/value/module_id.h"
#include "type/type.h"

namespace type {

struct Primitive : public LegacyType {
 public:
  enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  };

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
                     int32_t, int64_t, float, double, bool, ir::Char, Type,
                     ir::ModuleId, ir::addr_t /* TODO: Other primitives */>(
        std::forward<Fn>(fn));
  }

  base::MetaValue meta() const {
    return Apply([&]<typename T>()->base::MetaValue { return base::meta<T>; });
  }

  Completeness completeness() const override { return Completeness::Complete; }

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
    Primitive(Primitive::BasicType::I8),
    Primitive(Primitive::BasicType::I16),
    Primitive(Primitive::BasicType::I32),
    Primitive(Primitive::BasicType::I64),
    Primitive(Primitive::BasicType::F32),
    Primitive(Primitive::BasicType::F64),
    Primitive(Primitive::BasicType::Bool),
    Primitive(Primitive::BasicType::Char),
    Primitive(Primitive::BasicType::Type_),
    Primitive(Primitive::BasicType::Module),
    Primitive(Primitive::BasicType::MemPtr),
    Primitive(Primitive::BasicType::NullPtr),
    Primitive(Primitive::BasicType::EmptyArray),
    Primitive(Primitive::BasicType::Scope),
    Primitive(Primitive::BasicType::Block),
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

inline Type U8         = &(*internal::kPrimitiveArray)[0];
inline Type U16        = &(*internal::kPrimitiveArray)[1];
inline Type U32        = &(*internal::kPrimitiveArray)[2];
inline Type U64        = &(*internal::kPrimitiveArray)[3];
inline Type I8         = &(*internal::kPrimitiveArray)[4];
inline Type I16        = &(*internal::kPrimitiveArray)[5];
inline Type I32        = &(*internal::kPrimitiveArray)[6];
inline Type I64        = &(*internal::kPrimitiveArray)[7];
inline Type F32        = &(*internal::kPrimitiveArray)[8];
inline Type F64        = &(*internal::kPrimitiveArray)[9];
inline Type Bool       = &(*internal::kPrimitiveArray)[10];
inline Type Char       = &(*internal::kPrimitiveArray)[11];
inline Type Type_      = &(*internal::kPrimitiveArray)[12];
inline Type Module     = &(*internal::kPrimitiveArray)[13];
inline Type MemPtr     = &(*internal::kPrimitiveArray)[14];
inline Type NullPtr    = &(*internal::kPrimitiveArray)[15];
inline Type EmptyArray = &(*internal::kPrimitiveArray)[16];
inline Type Scope      = &(*internal::kPrimitiveArray)[17];
inline Type Block      = &(*internal::kPrimitiveArray)[18];
inline Type Label      = &(*internal::kPrimitiveArray)[19];
inline Type Interface  = &(*internal::kPrimitiveArray)[20];
inline Type Void       = &(*internal::kPrimitiveArray)[21];

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
  return p and p >= &I8.as<Primitive>() and p <= &F64.as<Primitive>();
}
inline bool IsFloatingPoint(Type t) {
  auto const *p = t.if_as<Primitive>();
  return p and p >= &F32.as<Primitive>() and p <= &F64.as<Primitive>();
}

}  // namespace type

#endif  // ICARUS_TYPE_PRIMITIVE_H
