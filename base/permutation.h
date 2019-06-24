#ifndef ICARUS_BASE_PERMUTATION_H
#define ICARUS_BASE_PERMUTATION_H

#include <vector>

#include "absl/random/distributions.h"

namespace base {

template <typename T, typename URBG>
void Shuffle(URBG&& urbg, absl::Span<T> span) {
  // TODO ASLR to make Fisher-Yates direction nondeterministic.
  std::shuffle(span.begin(), span.end(), std::forward<URBG>(urbg));
}

template <typename URBG>
std::vector<size_t> make_random_permutation(URBG&& urbg, size_t n) {
  std::vector<size_t> perm(n);
  std::iota(perm.begin(), perm.end(), 0);
  Shuffle(std::forward<URBG>(urbg), absl::MakeSpan(&perm.front(), perm.size()));
  return perm;
}

}  // namespace base

#endif  // ICARUS_BASE_PERMUTATION_H
