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

TEST(Value, Value) {
  EXPECT_TRUE(ir::Value(true).get<bool>());
  EXPECT_FALSE(ir::Value(false).get<bool>());
  EXPECT_EQ(ir::Value(3).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(3.0).get<double>(), 3.0);
}

TEST(Value, RegOr) {
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(3)).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(ir::RegOr<int32_t>(ir::Reg(3))).get<ir::Reg>(),
            ir::Reg(3));
}

TEST(Value, Ostream) {
  EXPECT_EQ(ToString(ir::Value(int64_t{3})), "3");
  EXPECT_EQ(ToString(ir::Value(ir::RegOr<int64_t>(3))), "3");
  EXPECT_EQ(ToString(ir::Value(ir::RegOr<int64_t>(ir::Reg(3)))), "r.3");
}
}  // namespace
