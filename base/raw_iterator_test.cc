#include "base/raw_iterator.h"

#include <cstring>

#include "gtest/gtest.h"

namespace base::internal {

TEST(Iterator, Read) {
  char buf[10];
  int32_t n = 123;
  std::memcpy(&buf[0], &n, sizeof(n));
  bool b = true;
  std::memcpy(&buf[4], &b, sizeof(b));
  n = 456;
  std::memcpy(&buf[5], &n, sizeof(n));
  b = false;
  std::memcpy(&buf[9], &b, sizeof(b));

  {
    raw_iterator iter(buf);
    EXPECT_EQ(iter.read<int>(), 123);
    EXPECT_TRUE(iter.read<bool>());
    EXPECT_EQ(iter.read<int>(), 456);
    EXPECT_FALSE(iter.read<bool>());
  }

  {
    raw_const_iterator iter(buf);
    EXPECT_EQ(iter.read<int>(), 123);
    EXPECT_TRUE(iter.read<bool>());
    EXPECT_EQ(iter.read<int>(), 456);
    EXPECT_FALSE(iter.read<bool>());
  }
}

TEST(Iterator, Skip) {
  char buf[10];
  int32_t n = 123;
  std::memcpy(&buf[0], &n, sizeof(n));
  bool b = true;
  std::memcpy(&buf[4], &b, sizeof(b));
  n = 456;
  std::memcpy(&buf[5], &n, sizeof(n));
  b = false;
  std::memcpy(&buf[9], &b, sizeof(b));

  {
    raw_iterator iter(buf);
    EXPECT_EQ(iter.read<int>(), 123);
    iter.skip(1);
    EXPECT_EQ(iter.read<int>(), 456);
    EXPECT_FALSE(iter.read<bool>());
  }

  {
    raw_const_iterator iter(buf);
    EXPECT_EQ(iter.read<int>(), 123);
    iter.skip(1);
    EXPECT_EQ(iter.read<int>(), 456);
    EXPECT_FALSE(iter.read<bool>());
  }
}

TEST(Iterator, ComparisonMutable) {
  char buf[10];

  raw_iterator lhs(buf);
  raw_iterator rhs(buf);

  EXPECT_EQ(lhs, rhs);
  lhs.skip(1);
  EXPECT_NE(lhs, rhs);
  EXPECT_GT(lhs, rhs);
  EXPECT_GE(lhs, rhs);
  rhs.skip(2);
  EXPECT_NE(lhs, rhs);
  EXPECT_LT(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  lhs.skip(1);
  EXPECT_EQ(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  EXPECT_GE(lhs, rhs);
}

TEST(Iterator, ComparisonMutableToConst) {
  char buf[10];

  raw_iterator lhs(buf);
  raw_const_iterator rhs(buf);

  EXPECT_EQ(lhs, rhs);
  lhs.skip(1);
  EXPECT_NE(lhs, rhs);
  EXPECT_GT(lhs, rhs);
  EXPECT_GE(lhs, rhs);
  rhs.skip(2);
  EXPECT_NE(lhs, rhs);
  EXPECT_LT(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  lhs.skip(1);
  EXPECT_EQ(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  EXPECT_GE(lhs, rhs);
}

TEST(Iterator, ComparisonConst) {
  char buf[10];

  raw_iterator lhs(buf);
  raw_const_iterator rhs(buf);

  EXPECT_EQ(lhs, rhs);
  lhs.skip(1);
  EXPECT_NE(lhs, rhs);
  EXPECT_GT(lhs, rhs);
  EXPECT_GE(lhs, rhs);
  rhs.skip(2);
  EXPECT_NE(lhs, rhs);
  EXPECT_LT(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  lhs.skip(1);
  EXPECT_EQ(lhs, rhs);
  EXPECT_LE(lhs, rhs);
  EXPECT_GE(lhs, rhs);
}

}  // namespace base::internal
