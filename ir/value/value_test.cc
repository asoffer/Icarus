#include "ir/value/value.h"

#include <cstdint>
#include <sstream>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/reg_or.h"

namespace {
using ::testing::ElementsAre;

TEST(Value, BasicConstruction) {
  EXPECT_TRUE(ir::Value(true).get<bool>());
  EXPECT_FALSE(ir::Value(false).get<bool>());
  EXPECT_EQ(ir::Value(3).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(3.0).get<double>(), 3.0);
}

TEST(Value, RegOrConstruction) {
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(3)).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(ir::Reg(3))).get<ir::Reg>(),
            ir::Reg(3));
}

TEST(Value, GetIf) {
  {
    ir::Value v(int32_t{3});
    ASSERT_NE(v.get_if<int32_t>(), nullptr);
    EXPECT_EQ(*v.get_if<int32_t>(), 3);
    EXPECT_EQ(v.get_if<bool>(), nullptr);
  }

  {
    ir::Value const v(int32_t{3});
    ASSERT_NE(v.get_if<int32_t>(), nullptr);
    EXPECT_EQ(*v.get_if<int32_t>(), 3);
    EXPECT_EQ(v.get_if<bool>(), nullptr);
  }
}

TEST(Value, Get) {
  {
    ir::Value v(int32_t{3});
    EXPECT_EQ(v.get<int32_t>(), 3);
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(3));
  }

  {
    ir::Value const v(int32_t{3});
    EXPECT_EQ(v.get<int32_t>(), 3);
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(3));
  }

  {
    ir::Value v(ir::Reg(3));
    EXPECT_EQ(v.get<ir::RegOr<int32_t>>(), ir::RegOr<int32_t>(ir::Reg(3)));
    EXPECT_EQ(v.get<ir::RegOr<bool>>(), ir::RegOr<bool>(ir::Reg(3)));
  }
}

}  // namespace
