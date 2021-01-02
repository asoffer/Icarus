#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

#include "type/type.h"

namespace type {

// Returns whether `from` is a type that can be reinterpreted in place into `to`.
bool CanCastInPlace(Type from, Type to);

// Returns whether `from` is a type that can be cast implicitly to `to`.
bool CanCastImplicitly(Type from, Type to);

// Returns whether `from` is a type that can be cast explicitly to `to`.
bool CanCastExplicitly(Type from, Type to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no meet exists.
Type Meet(Type lhs, Type rhs);

}  // namespace type

#endif  // ICARUS_TYPE_CAST_H
