#include "core/type_system/sized_integer.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

TEST(TypeSystem, SizedIntegerType) {
  using TS = TypeSystem<SizedIntegerType>;
  TS type_system;
  SizedIntegerType i32              = SizedIntegerType::I<TS>(32);
  SizedIntegerType u32              = SizedIntegerType::U<TS>(32);
  SizedIntegerType over_aligned_i32 = SizedIntegerType::I<TS>(32, Alignment(8));
  SizedIntegerType normally_aligned_i32 =
      SizedIntegerType::I<TS>(32, Alignment(4));

  EXPECT_EQ(i32, i32);
  EXPECT_NE(i32, u32);
  EXPECT_NE(i32, over_aligned_i32);
  EXPECT_EQ(i32, normally_aligned_i32);
}

TEST(SizedIntegerType, DefaultAlignment) {
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(1), Alignment(1));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(2), Alignment(1));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(7), Alignment(1));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(8), Alignment(1));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(9), Alignment(2));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(15), Alignment(2));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(16), Alignment(2));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(17), Alignment(4));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(24), Alignment(4));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(25), Alignment(4));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(32), Alignment(4));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(33), Alignment(8));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(48), Alignment(8));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(64), Alignment(8));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(65), Alignment(16));
  EXPECT_EQ(SizedIntegerType::DefaultAlignment(128), Alignment(16));
}

TEST(SizedIntegerType, Properties) {
  using TS = TypeSystem<SizedIntegerType>;

  SizedIntegerType u8 = SizedIntegerType::U<TS>(8);
  EXPECT_FALSE(u8.is_signed());
  EXPECT_EQ(u8.bits(), 8);
  EXPECT_EQ(u8.bytes(), Bytes(1));
  EXPECT_EQ(u8.alignment(), Alignment(1));

  SizedIntegerType u16 = SizedIntegerType::U<TS>(16);
  EXPECT_FALSE(u16.is_signed());
  EXPECT_EQ(u16.bits(), 16);
  EXPECT_EQ(u16.bytes(), Bytes(2));
  EXPECT_EQ(u16.alignment(), Alignment(2));

  SizedIntegerType u24 = SizedIntegerType::U<TS>(24);
  EXPECT_FALSE(u24.is_signed());
  EXPECT_EQ(u24.bits(), 24);
  EXPECT_EQ(u24.bytes(), Bytes(3));
  EXPECT_EQ(u24.alignment(), Alignment(4));

  SizedIntegerType u32 = SizedIntegerType::U<TS>(32);
  EXPECT_FALSE(u32.is_signed());
  EXPECT_EQ(u32.bits(), 32);
  EXPECT_EQ(u32.bytes(), Bytes(4));
  EXPECT_EQ(u32.alignment(), Alignment(4));

  SizedIntegerType u48 = SizedIntegerType::U<TS>(48);
  EXPECT_FALSE(u48.is_signed());
  EXPECT_EQ(u48.bits(), 48);
  EXPECT_EQ(u48.bytes(), Bytes(6));
  EXPECT_EQ(u48.alignment(), Alignment(8));

  SizedIntegerType u64 = SizedIntegerType::U<TS>(64);
  EXPECT_FALSE(u64.is_signed());
  EXPECT_EQ(u64.bits(), 64);
  EXPECT_EQ(u64.bytes(), Bytes(8));
  EXPECT_EQ(u64.alignment(), Alignment(8));

  SizedIntegerType u128 = SizedIntegerType::U<TS>(128);
  EXPECT_FALSE(u128.is_signed());
  EXPECT_EQ(u128.bits(), 128);
  EXPECT_EQ(u128.bytes(), Bytes(16));
  EXPECT_EQ(u128.alignment(), Alignment(16));

  SizedIntegerType i8 = SizedIntegerType::I<TS>(8);
  EXPECT_TRUE(i8.is_signed());
  EXPECT_EQ(i8.bits(), 8);
  EXPECT_EQ(i8.bytes(), Bytes(1));
  EXPECT_EQ(i8.alignment(), Alignment(1));

  SizedIntegerType i16 = SizedIntegerType::I<TS>(16);
  EXPECT_TRUE(i16.is_signed());
  EXPECT_EQ(i16.bits(), 16);
  EXPECT_EQ(i16.bytes(), Bytes(2));
  EXPECT_EQ(i16.alignment(), Alignment(2));

  SizedIntegerType i24 = SizedIntegerType::I<TS>(24);
  EXPECT_TRUE(i24.is_signed());
  EXPECT_EQ(i24.bits(), 24);
  EXPECT_EQ(i24.bytes(), Bytes(3));
  EXPECT_EQ(i24.alignment(), Alignment(4));

  SizedIntegerType i32 = SizedIntegerType::I<TS>(32);
  EXPECT_TRUE(i32.is_signed());
  EXPECT_EQ(i32.bits(), 32);
  EXPECT_EQ(i32.bytes(), Bytes(4));
  EXPECT_EQ(i32.alignment(), Alignment(4));

  SizedIntegerType i48 = SizedIntegerType::I<TS>(48);
  EXPECT_TRUE(i48.is_signed());
  EXPECT_EQ(i48.bits(), 48);
  EXPECT_EQ(i48.bytes(), Bytes(6));
  EXPECT_EQ(i48.alignment(), Alignment(8));

  SizedIntegerType i64 = SizedIntegerType::I<TS>(64);
  EXPECT_TRUE(i64.is_signed());
  EXPECT_EQ(i64.bits(), 64);
  EXPECT_EQ(i64.bytes(), Bytes(8));
  EXPECT_EQ(i64.alignment(), Alignment(8));

  SizedIntegerType i128 = SizedIntegerType::I<TS>(128);
  EXPECT_TRUE(i128.is_signed());
  EXPECT_EQ(i128.bits(), 128);
  EXPECT_EQ(i128.bytes(), Bytes(16));
  EXPECT_EQ(i128.alignment(), Alignment(16));
}

}  // namespace
}  // namespace core
