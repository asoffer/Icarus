#ifndef ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H
#define ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H

#include "base/meta.h"
#include "core/type_system/type_system.h"

namespace core {

// Represents a category of types that can be expressed via a finite set
// specified via an enum whose explicitly listed enumerators are the valid
// values for the type. Note that even though a value of an enumerator is valid
// when it is not explicitly listed, such values are not valid to be passed to
// constructors of `FiniteSetType` instantiations.
template <base::is_enum E>
struct FiniteSetType : TypeCategory<FiniteSetType<E>, E> {
  template <TypeSystemSupporting<FiniteSetType<E>> TS>
  explicit constexpr FiniteSetType(base::Meta<TS>, E e)
      : TypeCategory<FiniteSetType<E>, E>(base::meta<TS>, e) {}

  constexpr E value() const { return std::get<0>(this->decompose()); }
};

template <base::is_enum E>
FiniteSetType(E) -> FiniteSetType<E>;

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_FINITE_SET_H
