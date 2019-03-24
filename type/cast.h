#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

namespace type {
struct Type;

bool CanCast(Type const *from, Type const *to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no join exists.
Type const *Meet(Type const *lhs, Type const *rhs);

}  // namespace type

#endif // ICARUS_TYPE_CAST_H
