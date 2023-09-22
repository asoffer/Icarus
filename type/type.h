#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <cstring>
#include <vector>

namespace ic::type {

// All types definable within the type-system can be categorized by "kind" and
// fit into one of the kinds specified in "type/type_kind.xmacro.h".
#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "type/type_kind.xmacro.h"

// Objects of type `Type` represent types within the modeled type-system. The
// type `Type` is regular (i.e., can be safely copied, compared for equality,
// etc as one would expect `int` to be). Two `Type`s are considered equal if and
// only if they represent the same type in the type-system.
//
// `Type` is internally represented as a single `uint64_t`, and stores every
// other type-kind directly. Each such type-kind is responsible for setting its
// most-signficant byte to be the `uint8_t` encoding its kind. type-kind.
struct Type {
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
  Type() = default;
  uint64_t data_;
};

// Represents a primitive type built-in to the language.
struct PrimitiveType {
  enum class Kind : uint8_t {
#define IC_XMACRO_TOKEN_KIND_BUILTIN_TYPE(kind, symbol, spelling) kind,
#include "lexer/token_kind.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : data_((static_cast<uint64_t>(Type::Kind::Primitive) << 56) |
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

 private:
  friend Type;
  friend ParametersType Parameters(std::vector<ParametersType::Parameter>&&);
  friend ParametersType Parameters(
      std::vector<ParametersType::Parameter> const&);

  explicit ParametersType() = default;
  explicit constexpr ParametersType(uint64_t n)
      : data_((static_cast<uint64_t>(Type::Kind::Parameters) << 56) | n) {}

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

 private:
  friend Type;
  friend FunctionType Function(ParametersType, std::vector<Type>&&);
  friend FunctionType Function(ParametersType, std::vector<Type> const&);

  explicit FunctionType() = default;
  explicit constexpr FunctionType(uint64_t n)
      : data_((static_cast<uint64_t>(Type::Kind::Function) << 56) | n) {}

  uint64_t data_;
};

FunctionType Function(ParametersType pt, std::vector<Type>&& r);
FunctionType Function(ParametersType pt, std::vector<Type> const& r);

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
