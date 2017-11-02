#ifndef ICARUS_BASE_HASH_H
#define ICARUS_BASE_HASH_H

#include "types.h"

namespace base {
/*
template <typename Arg, typename... Args> struct Hash {
  size_t operator()(Arg&& arg, Args&& args...) const {
    return HashCombine(Hash<Args...>(std::forward<Args>(args)...), Hash(Arg));
  }
};
*/
} // namespace base

#endif // ICARUS_BASE_HASH_H
