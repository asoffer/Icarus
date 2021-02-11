#include "type/primitive.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"

namespace {

TEST(Primitive, Equality) {
  EXPECT_EQ(type::U8, type::U8);
  EXPECT_NE(type::U8, type::I8);
}

TEST(Primitive, ToString) {
  EXPECT_EQ(absl::StrFormat("%s", type::U8), "u8");
  EXPECT_EQ(absl::StrFormat("%s", type::Type_), "type");
}

}  // namespace
