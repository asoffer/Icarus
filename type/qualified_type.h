#ifndef ICARUS_TYPE_QUALIFIED_TYPE_H
#define ICARUS_TYPE_QUALIFIED_TYPE_H

#include <utility>

#include "type/qualifier.h"
#include "type/type.h"

namespace ic::type {

// All types definable within the type-system can be categorized by "kind" and
// fit into one of the kinds specified in "common/language/type_kind.xmacro.h".
// Each such type must be precisely 64-bits wide, and must have the
// most-significant 8 bytes be unset in any valid representation. Furthermore,
// the second-most-significant 8 bytes must be filled with a representation of
// the corresponding `Type::Kind` defined below.
#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "common/language/type_kind.xmacro.h"

struct QualifiedType {
  constexpr explicit QualifiedType() = default;

  static constexpr QualifiedType Unqualified(Type t) {
    return QualifiedType(Qualifier::Unqualified(), t);
  }

  static constexpr QualifiedType Constant(Type t) {
    return QualifiedType(Qualifier::Constant(), t);
  }

  constexpr explicit QualifiedType(Qualifier q, Type t)
      : qualifier_(q), type_(t) {}

  friend bool operator==(QualifiedType, QualifiedType) = default;

  bool constant() const { return qualifier() >= Qualifier::Constant(); }
  bool addressable() const { return qualifier() >= Qualifier::Addressable(); }

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType q) {
    return H::combine(std::move(h), q.qualifier_, q.type_);
  }

  friend void NthPrint(auto& p, auto& f, QualifiedType qt) {
    f(p, qt.qualifier());
    p.write(".(");
    f(p, qt.type());
    p.write(")");
  }

  [[nodiscard]] constexpr Qualifier qualifier() const { return qualifier_; }
  [[nodiscard]] constexpr Type type() const { return type_; }

 private:
  Qualifier qualifier_;
  Type type_;
};

}  // namespace ic::type

#endif  // ICARUS_TYPE_QUALIFIED_TYPE_H
