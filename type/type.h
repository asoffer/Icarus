#ifndef IC_TYPE_TYPE_H
#define IC_TYPE_TYPE_H

#include <array>
#include <concepts>
#include <cstring>
#include <utility>

#include "common/internal/constant_handle.h"
#include "type/type_contour.h"

namespace ic::type {

// Objects of type `Type` represent types within the modeled type-system. The
// type `Type` is regular (i.e., can be safely copied, compared for equality,
// etc as one would expect `int` to be). Two `Type`s are considered equal if and
// only if they represent the same type in the type-system. A `Type` is
// precisely 64-bits wide, and its most-significant 8 bits must always be unset.
struct Type : internal_constants::ConstantHandle<Type> {
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
    NTH_REQUIRE((v.harden), static_cast<size_t>(k) < Names.size());
    p.write("Type::Kind::");
    p.write(Names[static_cast<std::underlying_type_t<Kind>>(k)]);
  }

  bool NthSerialize(auto& s, Type t) {
    uint32_t n;
    static_assert(sizeof(n) == sizeof(t));
    std::memcpy(&n, &t, sizeof(n));
    return nth::io::write_fixed(s, n);
  }

  bool NthDeserialize(auto& d, Type& t) {
    uint32_t n;
    static_assert(sizeof(n) == sizeof(t));
    if (not nth::io::read_fixed(d, n)) { return false; }
    std::memcpy(&t, &n, sizeof(n));
    return true;
  }

  constexpr Kind kind() const { return static_cast<Kind>(value() & 0xff); }

  uint32_t index() const { return value() >> 8; }

  friend bool operator==(Type, Type) = default;

  template <std::derived_from<Type> T>
  T as() const {
    T t;
    std::memcpy(&t, this, sizeof(Type));
    return t;
  }

  friend void NthPrint(auto& p, auto& f, Type t) { NTH_UNIMPLEMENTED(); }

  struct from_index_t {};
  static constexpr from_index_t from_index;

  // TODO: Make this private.
  explicit Type(from_index_t, size_t index);
  explicit constexpr Type(Kind k, uint32_t data)
      : ConstantHandle((data << 8) | static_cast<uint8_t>(k)) {}
};

// Returns the number of `jasmin::Value`s required to hold a value of the given
// type `t`.
size_t JasminSize(Type t);

TypeContour Contour(Type t);

}  // namespace ic::type

#endif  // IC_TYPE_TYPE_H
