#include "core/type_system/sized_integer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, SizedIntegerType) {
  TypeSystem<SizedIntegerType> type_system;
  SizedIntegerType i32                  = SizedIntegerType::I<32>();
  SizedIntegerType u32                  = SizedIntegerType::U<32>();
  SizedIntegerType over_aligned_i32     = SizedIntegerType::I<32, 8>();
  SizedIntegerType normally_aligned_i32 = SizedIntegerType::I<32, 4>();

  EXPECT_EQ(i32, i32);
  EXPECT_NE(i32, u32);
  EXPECT_NE(i32, over_aligned_i32);
  EXPECT_EQ(i32, normally_aligned_i32);
}

}  // namespace
}  // namespace core
