#ifndef ICARUS_TYPE_CAST_H
#define ICARUS_TYPE_CAST_H

#include <numeric>

namespace type {
struct Type;

bool CanCast(Type const *from, Type const *to);
bool CanCastImplicitly(Type const *from, Type const *to);

// The 'meet' of two types is the maximal type that converts implicitly to both
// of these types. This is not guaranteed to exist, and the function returns
// nullptr if no join exists.
Type const *Meet(Type const *lhs, Type const *rhs);

// The 'join' of two types is the minimal type that both of the input types
// convert to. This is not guaranteed to exist, and the function returns nullptr
// if no join exists.
Type const *Join(Type const *lhs, Type const *rhs);

template <typename Container>
Type const *JoinAll(Container const &types) {
  return std::accumulate(types.begin(), types.end(), *types.begin(), Join);
}
}  // namespace type

#endif // ICARUS_TYPE_CAST_H
