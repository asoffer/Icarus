#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

#include "type/type.h"

namespace type {

bool CanCastImplicitly(Type from, Type to);
bool CanCast(Type from, Type to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no meet exists.
Type Meet(Type lhs, Type rhs);

}  // namespace type

#endif  // ICARUS_TYPE_CAST_H
