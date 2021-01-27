#include "base/iterator.h"

#include <vector>
#include "gtest/gtest.h"

namespace base {
namespace {

TEST(IteratorRange, Valid) {
  std::vector<int> v{1, 2, 3, 4};
  base::iterator_range range(v.begin(), v.end());
  int total = 0;
  for (int n : range) { total += n; }
  EXPECT_EQ(total, 10);
}

TEST(IteratorRange, Empty) {
  std::vector<int> v;
  base::iterator_range range(v.begin(), v.end());
  int total = 0;
  for (int n : range) { total += n; }
  EXPECT_EQ(total, 0);
}

struct iter {
  int operator*() const { return 2 * *it; }
  friend bool operator==(iter lhs, iter rhs) { return lhs.it == rhs.it; }
  friend bool operator!=(iter lhs, iter rhs) { return not(lhs == rhs); }
  iter operator++() {
    ++it;
    return *this;
  }

  std::vector<int>::iterator it;
};

TEST(IteratorRange, CustomIterator) {
  std::vector<int> v{1, 2, 3, 4};
  base::iterator_range range(iter{.it = v.begin()}, iter{.it = v.end()});
  int total = 0;
  for (int n : range) { total += n; }
  EXPECT_EQ(total, 20);
}

}  // namespace
}  // namespace base
