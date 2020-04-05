#include "ir/value/value.h"

#include <sstream>
#include <cstdint>

#include "gtest/gtest.h"
#include "ir/value/reg_or.h"

namespace {

std::string ToString(ir::Value val) {
  std::stringstream ss;
  ss << val;
  return ss.str();
}

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

TEST(Value, Ostream) {
  EXPECT_EQ(ToString(ir::Value(int64_t{3})), "3");
  EXPECT_EQ(ToString(ir::Value(ir::RegOr<int64_t>(3))), "3");
  EXPECT_EQ(ToString(ir::Value(ir::RegOr<int64_t>(ir::Reg(3)))), "r.3");
}
}  // namespace
