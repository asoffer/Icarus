#ifndef ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H
#define ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H

#include "core/type_system/type_system.h"
#include "nth/meta/concepts.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace core {

// Represents a category of types that can be expressed via a finite set
// specified via an enum whose explicitly listed enumerators are the valid
// values for the type. Note that even though a value of an enumerator is valid
// when it is not explicitly listed, such values are not valid to be passed to
// constructors of `FiniteSetType` instantiations.
template <nth::enumeration E>
struct FiniteSetType : TypeCategory<FiniteSetType<E>, E> {
  explicit constexpr FiniteSetType(nth::Type auto t, E e) requires
      TypeSystemSupporting<typename decltype(t)::type, FiniteSetType<E>>
      : TypeCategory<FiniteSetType<E>, E>(t, e) {}

  constexpr E value() const { return std::get<0>(this->decompose()); }
};

template <nth::enumeration E>
FiniteSetType(E) -> FiniteSetType<E>;

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H
