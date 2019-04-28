#ifndef ICARUS_BASE_PERMUTATION_H
#define ICARUS_BASE_PERMUTATION_H

#include <random>
#include <vector>

namespace base {

inline std::vector<size_t> make_random_permutation(size_t n) {
  std::vector<size_t> perm(n);
  std::iota(perm.begin(), perm.end(), 0);
  std::shuffle(perm.begin(), perm.end(), std::random_device{});
  return perm;
}

}  // namespace base

#endif  // ICARUS_BASE_PERMUTATION_H
