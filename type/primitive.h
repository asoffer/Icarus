#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "absl/functional/function_ref.h"
#include "base/global.h"
#include "base/meta.h"
#include "type/type.h"

namespace type {

struct Primitive : public Type {
 public:
  enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  };

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

  base::MetaValue meta() const {
    base::MetaValue result = base::meta<void>;
    Apply([&](auto m) { result = m; });
    return result;
  }

  template <typename Fn>
  void Apply(Fn &&fn) const {
    ApplyImpl<uint8_t, uint16_t, uint32_t, uint64_t, int8_t, int16_t, int32_t,
              int64_t, float, double, bool,
              Type const * /* TODO: Other primitives */>(std::forward<Fn>(fn));
  }

  Completeness completeness() const override { return Completeness::Complete; }

  bool is_big() const override { return false; }
  bool is_integral() const;

  BasicType type_;

 private:
  template <typename... Ts, typename Fn>
  void ApplyImpl(Fn &&fn) const;
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
void Primitive::ApplyImpl(Fn &&fn) const {
  // Because primitive types are unique, we can compare the address to
  // `kPrimitiveArray->data()` and use the offset to index into a collection of
  // function of our own creation.
  int index = static_cast<int>(this - internal::kPrimitiveArray->data());
  std::array{absl::FunctionRef<void()>(
      [&] { std::forward<Fn>(fn)(base::meta<Ts>); })...}[index]();
}

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
