#include "core/type_system/builtin.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, Builtin) {
  TypeSystem<BuiltinType> type_system;
  BuiltinType i32                  = BuiltinType::I<32>(type_system);
  BuiltinType u32                  = BuiltinType::U<32>(type_system);
  BuiltinType over_aligned_i32     = BuiltinType::I<32, 8>(type_system);
  BuiltinType normally_aligned_i32 = BuiltinType::I<32, 4>(type_system);

  EXPECT_EQ(i32, i32);
  EXPECT_NE(i32, u32);
  EXPECT_NE(i32, over_aligned_i32);
  EXPECT_EQ(i32, normally_aligned_i32);
}

}  // namespace
}  // namespace core
