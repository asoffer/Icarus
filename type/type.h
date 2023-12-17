#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <cstring>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

#include "jasmin/core/value.h"
#include "nth/container/flyweight_set.h"
#include "type/type_contour.h"
#include "type/type_system.pb.h"

namespace ic::type {

// All types definable within the type-system can be categorized by "kind" and
// fit into one of the kinds specified in "common/language/type_kind.xmacro.h".
// Each such type must be precisely 64-bits wide, and must have the
// most-significant 8 bytes be unset in any valid representation. Furthermore,
// the second-most-significant 8 bytes must be filled with a representation of
// the corresponding `Type::Kind` defined below.
#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "common/language/type_kind.xmacro.h"

// Objects of type `Type` represent types within the modeled type-system. The
// type `Type` is regular (i.e., can be safely copied, compared for equality,
// etc as one would expect `int` to be). Two `Type`s are considered equal if and
// only if they represent the same type in the type-system. A `Type` is
// precisely 64-bits wide, and its most-significant 8 bits must always be unset.
struct Type {
  Type() = default;

  enum class Kind : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind,
#include "common/language/type_kind.xmacro.h"
  };

  friend void NthPrint(auto& p, auto&, Kind k) {
    static constexpr std::array Names = {
#define IC_XMACRO_TYPE_KIND(kind) #kind,
#include "common/language/type_kind.xmacro.h"
    };
    p.write("Type::Kind::");
    p.write(Names[static_cast<std::underlying_type_t<Kind>>(k)]);
  }

  constexpr Kind kind() const {
    return static_cast<Kind>((data_ >> 48) & 0xff);
  }

  uint64_t index() const { return data_ & uint64_t{0x0000'ffff'ffff'ffff}; }

  friend bool operator==(Type, Type) = default;

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.data_);
  }

  friend bool IcarusDeserializeValue(std::span<jasmin::Value const> values,
                                     Type& t) {
    if (values.size() != 1) { return false; }
    t.data_ = values.front().raw_value();
    return true;
  }

  // Defines implicit constructors from each specific type-kind, as well as
  // functions which convert to specific types. These functions are all named by
  // the specific type-kind prefixed with "As", so that `AsSpecificType` would
  // convert to `SpecificType`.
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  constexpr Type(kind##Type t);                                                \
  kind##Type As##kind() const;
#include "common/language/type_kind.xmacro.h"

  friend void NthPrint(auto& p, auto& f, Type t);

 private:
  friend struct QualifiedType;

  explicit constexpr Type(uint64_t data) : data_(data) {}

  uint64_t data_;
};

struct Qualifier {
  static constexpr Qualifier Constant() { return Qualifier(1); }
  static constexpr Qualifier Unqualified() { return Qualifier(0); }

  friend constexpr bool operator==(Qualifier, Qualifier) = default;
  friend constexpr bool operator!=(Qualifier, Qualifier) = default;
  friend constexpr bool operator<=(Qualifier lhs, Qualifier rhs) {
    return (lhs.data_ & rhs.data_) == lhs.data_;
  }
  friend constexpr bool operator>=(Qualifier lhs, Qualifier rhs) {
    return rhs <= lhs;
  }

  template <typename H>
  friend H AbslHashValue(H h, Qualifier q) {
    H::combine(std::move(h), q.data_);
  }

  friend void NthPrint(auto& p, auto&, Qualifier q) {
    if (q.data_ == 1) { p.write("c"); }
  }

 private:
  friend struct QualifiedType;
  explicit constexpr Qualifier(uint8_t data) : data_(data) {}

  uint8_t data_;
};

struct QualifiedType {
  constexpr explicit QualifiedType() = default;

  static constexpr QualifiedType Unqualified(Type t) {
    return QualifiedType(Qualifier::Unqualified(), t);
  }

  static constexpr QualifiedType Constant(Type t) {
    return QualifiedType(Qualifier::Constant(), t);
  }

  constexpr explicit QualifiedType(Qualifier q, Type t)
      : data_(static_cast<uint64_t>(q.data_) << 56 | t.data_) {}

  friend bool operator==(QualifiedType, QualifiedType) = default;

  bool constant() const { return qualifier() >= Qualifier::Constant(); }

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType q) {
    return H::combine(std::move(h), q.data_);
  }

  friend void NthPrint(auto& p, auto& f, QualifiedType qt) {
    f(p, qt.qualifier());
    p.write(".(");
    f(p, qt.type());
    p.write(")");
  }

  constexpr Qualifier qualifier() const { return Qualifier(data_ >> 56); }
  constexpr Type type() const {
    return Type(data_ & uint64_t{0x00ffffff'ffffffff});
  }

 private:
  uint64_t data_;
};

namespace internal_type {

struct BasicType {
  explicit BasicType() = default;
  explicit constexpr BasicType(Type::Kind k, uint64_t n)
      : data_((static_cast<uint64_t>(k) << 48) | n) {}

  friend bool operator==(BasicType, BasicType) = default;
  friend bool operator!=(BasicType, BasicType) = default;

  template <typename H>
  friend H AbslHashValue(H h, std::derived_from<BasicType> auto t) {
    return H::combine(std::move(h), t.data_);
  }

 protected:
  uint64_t data() const { return data_ & uint64_t{0x0000ffff'ffffffff}; }

 private:
  friend Type;
  uint64_t data_;
};

}  // namespace internal_type

// Represents a primitive type built-in to the language.
struct PrimitiveType : internal_type::BasicType {
  enum class Kind : uint8_t {
#define IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY(name)                          \
  InternalBeginCategory_##name,
#define IC_XMACRO_PRIMITIVE_TYPE_END_CATEGORY(name) InternalEndCategory_##name,
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling) kind,
#include "common/language/primitive_types.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : internal_type::BasicType(Type::Kind::Primitive,
                                 static_cast<uint64_t>(k)) {}

  Kind kind() const { return static_cast<Kind>(data() & 0xff); }

  friend void NthPrint(auto& p, auto& f, PrimitiveType t) {
    switch (t.kind()) {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Kind::kind:                                                             \
    p.write(spelling);                                                         \
    return;
#include "common/language/primitive_types.xmacro.h"
      default: NTH_UNREACHABLE();
    }
  }

 private:
  friend Type;
  PrimitiveType() = default;
};

#define IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY(category_name)                 \
  bool category_name(PrimitiveType);
#include "common/language/primitive_types.xmacro.h"

// Represents a set of parameters to an invocable type.
struct ParametersType : internal_type::BasicType {
  struct Parameter {
    uint64_t name;
    Type type;

    friend bool operator==(Parameter, Parameter) = default;

    template <typename H>
    friend H AbslHashValue(H h, Parameter p) {
      return H::combine(std::move(h), p.name, p.type);
    }

    friend void NthPrint(auto& p, auto& fmt, Parameter param) {
      fmt(p, param.name);
      p.write(": ");
      fmt(p, param.type);
    }
  };

  size_t size() const;
  std::vector<Parameter> const& operator*() const;

  friend void NthPrint(auto& p, auto& fmt, ParametersType params) {
    std::string_view separator = "";
    for (auto const& param : *params) {
      p.write(std::exchange(separator, ", "));
      fmt(p, param);
    }
  }

 private:
  friend Type;
  friend ParametersType Parameters(std::vector<ParametersType::Parameter>&&);
  friend ParametersType Parameters(
      std::vector<ParametersType::Parameter> const&);

  explicit ParametersType() = default;
  explicit constexpr ParametersType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Parameters, n) {}
};

ParametersType Parameters(std::vector<ParametersType::Parameter>&& p);
ParametersType Parameters(std::vector<ParametersType::Parameter> const& p);

enum Evaluation {
  RequireCompileTime,
  PreferCompileTime,
  PreferRuntime,
  RequireRuntime,
};

struct FunctionType : internal_type::BasicType {
  explicit FunctionType() = default;

  Evaluation evaluation() const;
  ParametersType parameters() const;
  std::vector<Type> const& returns() const;

  friend void NthPrint(auto& p, auto& fmt, FunctionType f) {
    std::string_view separator = "";
    p.write("(");
    fmt(p, f.parameters());
    std::span returns = f.returns();
    p.write(returns.size() != 1 ? ") -> (" : ") -> ");
    separator = "";
    for (auto const& r : f.returns()) {
      p.write(std::exchange(separator, ", "));
      fmt(p, r);
    }
    if (returns.size() != 1) { p.write(")"); }
  }

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto&);
  friend void DeserializeTypeSystem(TypeSystemProto const&);
  friend FunctionType Function(ParametersType, std::vector<Type>&&, Evaluation);
  friend FunctionType Function(ParametersType, std::vector<Type> const&,
                               Evaluation);

  explicit constexpr FunctionType(uint64_t n)
      : internal_type::BasicType(Type::Kind::Function, n) {}
};

FunctionType Function(ParametersType pt, std::vector<Type>&& r,
                      Evaluation e = Evaluation::PreferRuntime);
FunctionType Function(ParametersType pt, std::vector<Type> const& r,
                      Evaluation e = Evaluation::PreferRuntime);

struct PointerType : internal_type::BasicType {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, PointerType ptr) {
    p.write("*");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend PointerType Ptr(Type);

  explicit PointerType() = default;
  explicit constexpr PointerType(uint64_t n)
      : BasicType(Type::Kind::Pointer, n) {}
};

PointerType Ptr(Type t);

struct BufferPointerType : internal_type::BasicType {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, BufferPointerType ptr) {
    p.write("[*]");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend BufferPointerType BufPtr(Type);

  explicit BufferPointerType() = default;
  explicit constexpr BufferPointerType(uint64_t n)
      : BasicType(Type::Kind::BufferPointer, n) {}
};

BufferPointerType BufPtr(Type t);

struct PatternType : internal_type::BasicType {
  Type match_type() const;

 private:
  friend Type;
  friend PatternType Pattern(Type);

  explicit PatternType() = default;
  explicit constexpr PatternType(uint64_t n)
      : BasicType(Type::Kind::Pattern, n) {}
};

PatternType Pattern(Type t);

struct SliceType : internal_type::BasicType {
  Type element_type() const;

  friend void NthPrint(auto& p, auto& f, SliceType s) {
    p.write("\\");
    f(p, s.element_type());
  }

 private:
  friend Type;
  friend void SerializeTypeSystem(TypeSystemProto& );
  friend void DeserializeTypeSystem(TypeSystemProto const& );
  friend SliceType Slice(Type);

  explicit SliceType() = default;
  explicit constexpr SliceType(uint64_t n) : BasicType(Type::Kind::Slice, n) {}
};

SliceType Slice(Type t);

struct GenericFunctionType : internal_type::BasicType {
  Evaluation evaluation() const;
  void const* data() const;

 private:
  friend Type;
  friend GenericFunctionType GenericFunction(Evaluation e, void const* fn);

  explicit GenericFunctionType() = default;
  explicit constexpr GenericFunctionType(uint64_t n)
      : BasicType(Type::Kind::GenericFunction, n) {}
};

GenericFunctionType GenericFunction(Evaluation e, void const* fn);

struct OpaqueType : internal_type::BasicType {

  friend void NthPrint(auto& p, auto& fmt, OpaqueType o) {
    p.write("opaque.");
    fmt(p, type::Type(o).index());
  }

 private:
  friend Type;
  friend void Serialize(Type type, TypeProto& proto);
  friend Type Deserialize(TypeProto const&, TypeSystemProto const&);
  friend OpaqueType Opaque();

  explicit OpaqueType() = default;
  explicit constexpr OpaqueType(uint64_t n)
      : BasicType(Type::Kind::Opaque, n) {}
};

OpaqueType Opaque();

#define IC_XMACRO_TYPE_KIND(kind)                                              \
  inline constexpr Type::Type(kind##Type t) : data_(t.data_) {}
#include "common/language/type_kind.xmacro.h"

#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  inline constexpr Type symbol = PrimitiveType(PrimitiveType::Kind::kind);
#include "common/language/primitive_types.xmacro.h"

void NthPrint(auto& p, auto& f, Type t) {
  switch (t.kind()) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Type::Kind::kind:                                                       \
    f(p, t.As##kind());                                                        \
    return;
#include "common/language/type_kind.xmacro.h"
  }
}

// Returns the number of `jasmin::Value`s required to hold a value of the given
// type `t`.
size_t JasminSize(Type t);

struct TypeSystem {
  nth::flyweight_set<std::vector<ParametersType::Parameter>> parameters;
  nth::flyweight_set<std::vector<Type>> returns;

  nth::flyweight_set<std::tuple<ParametersType, uint64_t, Evaluation>>
      functions;
  nth::flyweight_set<Type> slice_element_types;
  nth::flyweight_set<Type> pointee_types;
  nth::flyweight_set<Type> buffer_pointee_types;
  nth::flyweight_set<Type> pattern_types;
  nth::flyweight_set<std::pair<void const*, Evaluation>> generic_function_types;
};

TypeContour Contour(Type t);

TypeSystem const& GlobalTypeSystem();

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_H
