#include "base/defer.h"

#include "gtest/gtest.h"

namespace base {

TEST(Defer, Works) {
  int n = 0;
  {
    base::defer d([&] { --n; });
    EXPECT_EQ(n, 0);
    n += 3;
    EXPECT_EQ(n, 3);
  }
  EXPECT_EQ(n, 2);
}

}  // namespace base
