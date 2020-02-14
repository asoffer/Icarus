#include "ir/value/enum_and_flags.h"

#include "gtest/gtest.h"

namespace ir {
namespace {

TEST(Flags, BitwiseOperators) {
  EXPECT_EQ((FlagsVal(3) | FlagsVal(5)), FlagsVal(7));
  EXPECT_EQ((FlagsVal(3) & FlagsVal(5)), FlagsVal(1));
  EXPECT_EQ((FlagsVal(3) ^ FlagsVal(5)), FlagsVal(6));

  EXPECT_EQ((FlagsVal(3) | FlagsVal(3)), FlagsVal(3));
  EXPECT_EQ((FlagsVal(3) & FlagsVal(3)), FlagsVal(3));
  EXPECT_EQ((FlagsVal(3) ^ FlagsVal(3)), FlagsVal());
}

TEST(Flags, Comparison) {
  EXPECT_LT(FlagsVal(3), FlagsVal(7));
  EXPECT_LE(FlagsVal(3), FlagsVal(7));
  EXPECT_LE(FlagsVal(7), FlagsVal(7));

  EXPECT_GT(FlagsVal(7), FlagsVal(3));
  EXPECT_GE(FlagsVal(7), FlagsVal(3));
  EXPECT_GE(FlagsVal(7), FlagsVal(7));

  EXPECT_NE(FlagsVal(7), FlagsVal(3));
}

}  // namespace
}  // namespace ir
