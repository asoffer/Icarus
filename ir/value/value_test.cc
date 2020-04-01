#include "ir/value/value.h"

#include <cstdint>

#include "gtest/gtest.h"

namespace {

TEST(Value, Value) {
  EXPECT_TRUE(ir::Value(true).get<bool>());
  EXPECT_FALSE(ir::Value(false).get<bool>());
  EXPECT_EQ(ir::Value(3).get<int32_t>(), 3);
  EXPECT_EQ(ir::Value(3.0).get<double>(), 3.0);
}

}  // namespace
