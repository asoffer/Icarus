#include "base/move_func.h"

#include "gtest/gtest.h"

TEST(MoveFunc, Call) {
  base::move_func<int(int)> f = [](int n) { return n * n; };
  EXPECT_EQ(std::move(f)(3), 9);
  EXPECT_EQ(f, nullptr);
}

TEST(MoveFunc, CastToBool) {
  base::move_func<int(int)> f = [](int n) { return n * n; };
  EXPECT_TRUE(f);
  auto g = std::move(f);
  EXPECT_FALSE(f);
  EXPECT_TRUE(g);
}
