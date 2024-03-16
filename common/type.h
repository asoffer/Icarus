#ifndef IC_COMMON_TYPE_H
#define IC_COMMON_TYPE_H

#include <array>
#include <utility>

#include "common/internal/constant_handle.h"

namespace ic::type {

#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "common/language/type_kind.xmacro.h"

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

  // Defines implicit constructors from each specific type-kind, as well as
  // functions which convert to specific types. These functions are all named by
  // the specific type-kind prefixed with "As", so that `AsSpecificType` would
  // convert to `SpecificType`.
  Type(OpaqueType t);
  Type(RefinementType t);
  Type(DependentFunctionType t);
#define IC_XMACRO_TYPE_KIND(kind) kind##Type As##kind() const;
#include "common/language/type_kind.xmacro.h"

  friend void NthPrint(auto& p, auto& f, Type t);

  struct from_index_t {};
  static constexpr from_index_t from_index;

  // TODO: Make this private.
  explicit Type(from_index_t, size_t index);
  explicit constexpr Type(Kind k, uint32_t data)
      : ConstantHandle((data << 8) | static_cast<uint8_t>(k)) {}
};

}  // namespace ic::type

#endif  // IC_COMMON_TYPE_H
