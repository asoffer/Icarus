#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <cstring>
#include <vector>

namespace ic::type {

// All types definable within the type-system can be categorized by "kind" and
// fit into one of the kinds specified in "type/type_kind.xmacro.h". Each such
// type must be precisely 64-bits wide, and must reserve have the
// most-significant 8 bytes be unset in any valid representation. Furthermore,
// the second-most-significant 8 bytes must be filled with a representation of
// the corresponding `Type::Kind` defined below.
#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "type/type_kind.xmacro.h"

// Objects of type `Type` represent types within the modeled type-system. The
// type `Type` is regular (i.e., can be safely copied, compared for equality,
// etc as one would expect `int` to be). Two `Type`s are considered equal if and
// only if they represent the same type in the type-system. A `Type` is
// precisely 64-bits wide, and its most-significant 8 bits must always be unset.
struct Type {
  Type() = default;

  enum class Kind : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind,
#include "type/type_kind.xmacro.h"
  };

  friend void NthPrint(auto& p, auto&, Kind k) {
    switch (k) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Kind::kind:                                                             \
    p.write("Type::Kind::");                                                   \
    p.write(#kind);                                                            \
    break;
#include "type/type_kind.xmacro.h"
    }
  }

  Kind kind() const;

  friend bool operator==(Type, Type) = default;

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.data_);
  }

  // Defines implicit constructors from each specific type-kind, as well as
  // functions which convert to specific types. These functions are all named by
  // the specific type-kind prefixed with "As", so that `AsSpecificType` would
  // convert to `SpecificType`.
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  constexpr Type(kind##Type t);                                                \
  kind##Type As##kind() const;
#include "type/type_kind.xmacro.h"

  friend void NthPrint(auto& p, auto& f, Type t);

 private:
  friend struct QualifiedType;

  explicit constexpr Type(uint64_t data) : data_(data) {}

  uint64_t data_;
};

struct Qualifier {
  static constexpr Qualifier Constant() { return Qualifier(1); }
  static constexpr Qualifier Unqualified() { return Qualifier(0); }

  friend bool operator==(Qualifier, Qualifier) = default;

  template <typename H>
  friend H AbslHashValue(H h, Qualifier q) {
    H::combine(std::move(h), q.data_);
  }

 private:
  friend struct QualifiedType;
  explicit constexpr Qualifier(uint8_t data) : data_(data) {}

  uint8_t data_;
};

struct QualifiedType {
  constexpr explicit QualifiedType() = default;

  constexpr explicit QualifiedType(Qualifier q, Type t)
      : data_(static_cast<uint64_t>(q.data_) << 56 | t.data_) {}

  friend bool operator==(QualifiedType,QualifiedType) = default;

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType q) {
    return H::combine(std::move(h), q.data_);
  }

  constexpr Qualifier qualifier() const { return Qualifier(data_ >> 56); }
  constexpr Type type() const {
    return Type(data_ & uint64_t{0x00ffffff'ffffffff});
  }

 private:
  uint64_t data_;
};

// Represents a primitive type built-in to the language.
struct PrimitiveType {
  enum class Kind : uint8_t {
#define IC_XMACRO_TOKEN_KIND_BUILTIN_TYPE(kind, symbol, spelling) kind,
#include "lexer/token_kind.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : data_((static_cast<uint64_t>(Type::Kind::Primitive) << 48) |
               static_cast<uint64_t>(k)) {}

  friend bool operator==(PrimitiveType, PrimitiveType) = default;

  Kind kind() const { return static_cast<Kind>(data_ & 0xff); }

  friend void NthPrint(auto& p, auto& f, PrimitiveType t) {
    switch (t.kind()) {
#define IC_XMACRO_TOKEN_KIND_BUILTIN_TYPE(kind, symbol, spelling)              \
  case Kind::kind:                                                             \
    p.write(spelling);                                                         \
    return;
#include "lexer/token_kind.xmacro.h"
    }
  }

 private:
  friend Type;
  PrimitiveType() = default;

  uint64_t data_;
};

// Represents a set of parameters to an invocable type.
struct ParametersType {
  struct Parameter {
    uint64_t name;
    Type type;

    friend bool operator==(Parameter, Parameter) = default;

    template <typename H>
    friend H AbslHashValue(H h, Parameter p) {
      return H::combine(std::move(h), p.name, p.type);
    }
  };

  friend bool operator==(ParametersType, ParametersType) = default;

  template <typename H>
  friend H AbslHashValue(H h, ParametersType t) {
    return H::combine(std::move(h), t.data_);
  }

  std::vector<Parameter> const& operator*() const;

 private:
  friend Type;
  friend ParametersType Parameters(std::vector<ParametersType::Parameter>&&);
  friend ParametersType Parameters(
      std::vector<ParametersType::Parameter> const&);

  explicit ParametersType() = default;
  explicit constexpr ParametersType(uint64_t n)
      : data_((static_cast<uint64_t>(Type::Kind::Parameters) << 48) | n) {}

  uint64_t data() const { return data_ & uint64_t{0x0000ffff'ffffffff}; }

  uint64_t data_;
};

ParametersType Parameters(std::vector<ParametersType::Parameter>&& p);
ParametersType Parameters(std::vector<ParametersType::Parameter> const& p);

struct FunctionType {
  friend bool operator==(FunctionType, FunctionType) = default;

  template <typename H>
  friend H AbslHashValue(H h, FunctionType t) {
    return H::combine(std::move(h), t.data_);
  }

  ParametersType parameters() const;
  std::vector<Type> const& returns() const;

 private:
  friend Type;
  friend FunctionType Function(ParametersType, std::vector<Type>&&);
  friend FunctionType Function(ParametersType, std::vector<Type> const&);

  explicit FunctionType() = default;
  explicit constexpr FunctionType(uint64_t n)
      : data_((static_cast<uint64_t>(Type::Kind::Function) << 48) | n) {}

  uint64_t data() const { return data_ & uint64_t{0x0000ffff'ffffffff}; }

  uint64_t data_;
};

FunctionType Function(ParametersType pt, std::vector<Type>&& r);
FunctionType Function(ParametersType pt, std::vector<Type> const& r);

struct SliceType {
  friend bool operator==(SliceType, SliceType) = default;

  template <typename H>
  friend H AbslHashValue(H h, SliceType t) {
    return H::combine(std::move(h), t.data_);
  }

  Type element_type() const;

 private:
  friend Type;
  friend SliceType Slice(Type);

  explicit SliceType() = default;
  explicit constexpr SliceType(uint64_t n)
      : data_((static_cast<uint64_t>(Type::Kind::Slice) << 48) | n) {}

  uint64_t data() const { return data_ & uint64_t{0x0000ffff'ffffffff}; }

  uint64_t data_;
};

SliceType Slice(Type t);

#define IC_XMACRO_TYPE_KIND(kind)                                              \
  inline constexpr Type::Type(kind##Type t) : data_(t.data_) {}
#include "type/type_kind.xmacro.h"

#define IC_XMACRO_TOKEN_KIND_BUILTIN_TYPE(kind, symbol, spelling)              \
  inline constexpr Type symbol = PrimitiveType(PrimitiveType::Kind::kind);
#include "lexer/token_kind.xmacro.h"

void NthPrint(auto& p, auto& f, Type t) {
  switch (t.kind()) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Type::Kind::kind:                                                       \
    f(p, t.As##kind());                                                        \
    return;
#include "type/type_kind.xmacro.h"
  }
}

size_t Size(Type t);

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_H
