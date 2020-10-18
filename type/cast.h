#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

namespace type {
struct LegacyType;

bool CanCastImplicitly(LegacyType const *from, LegacyType const *to);
bool CanCast(LegacyType const *from, LegacyType const *to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no meet exists.
LegacyType const *Meet(LegacyType const *lhs, LegacyType const *rhs);

}  // namespace type

#endif  // ICARUS_TYPE_CAST_H
