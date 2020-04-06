#include "ir/value/builtin_fn.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/primitive.h"

namespace {
using ::testing::Optional;

TEST(BuiltinFn, Equality) {
  EXPECT_EQ(ir::BuiltinFn::Bytes(), ir::BuiltinFn::Bytes());
  EXPECT_NE(ir::BuiltinFn::Bytes(), ir::BuiltinFn::Alignment());
}

TEST(BuiltinFn, ByName) {
  EXPECT_THAT(ir::BuiltinFn::ByName("foreign"),
              Optional(ir::BuiltinFn::Foreign()));

  EXPECT_EQ(ir::BuiltinFn::ByName("FOREIGN"), std::nullopt);
}

}  // namespace
