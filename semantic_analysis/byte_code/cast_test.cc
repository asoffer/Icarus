#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Cast, Integer) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<int8_t>(R"(17 as i8)"), 17);
  EXPECT_EQ(repl.execute<int16_t>(R"(17 as i16)"), 17);
  EXPECT_EQ(repl.execute<int32_t>(R"(17 as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"(17 as i64)"), 17);
  EXPECT_EQ(repl.execute<uint8_t>(R"(17 as u8)"), 17);
  EXPECT_EQ(repl.execute<uint16_t>(R"(17 as u16)"), 17);
  EXPECT_EQ(repl.execute<uint32_t>(R"(17 as u32)"), 17);
  EXPECT_EQ(repl.execute<uint64_t>(R"(17 as u64)"), 17);
}

TEST(Cast, SizedIntegerTypes) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<int8_t>(R"((17 as i8) as i8)"), 17);
  EXPECT_EQ(repl.execute<int16_t>(R"((17 as i8) as i16)"), 17);
  EXPECT_EQ(repl.execute<int32_t>(R"((17 as i8) as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"((17 as i8) as i64)"), 17);

  EXPECT_EQ(repl.execute<int16_t>(R"((17 as u8) as i16)"), 17);
  EXPECT_EQ(repl.execute<int32_t>(R"((17 as u8) as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"((17 as u8) as i64)"), 17);
  EXPECT_EQ(repl.execute<uint8_t>(R"((17 as u8) as u8)"), 17);
  EXPECT_EQ(repl.execute<uint16_t>(R"((17 as u8) as u16)"), 17);
  EXPECT_EQ(repl.execute<uint32_t>(R"((17 as u8) as u32)"), 17);
  EXPECT_EQ(repl.execute<uint64_t>(R"((17 as u8) as u64)"), 17);

  EXPECT_EQ(repl.execute<int16_t>(R"((17 as i16) as i16)"), 17);
  EXPECT_EQ(repl.execute<int32_t>(R"((17 as i16) as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"((17 as i16) as i64)"), 17);

  EXPECT_EQ(repl.execute<int32_t>(R"((17 as u16) as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"((17 as u16) as i64)"), 17);
  EXPECT_EQ(repl.execute<uint16_t>(R"((17 as u16) as u16)"), 17);
  EXPECT_EQ(repl.execute<uint32_t>(R"((17 as u16) as u32)"), 17);
  EXPECT_EQ(repl.execute<uint64_t>(R"((17 as u16) as u64)"), 17);

  EXPECT_EQ(repl.execute<int32_t>(R"((17 as i32) as i32)"), 17);
  EXPECT_EQ(repl.execute<int64_t>(R"((17 as i32) as i64)"), 17);

  EXPECT_EQ(repl.execute<int64_t>(R"((17 as u32) as i64)"), 17);
  EXPECT_EQ(repl.execute<uint32_t>(R"((17 as u32) as u32)"), 17);
  EXPECT_EQ(repl.execute<uint64_t>(R"((17 as u32) as u64)"), 17);

  EXPECT_EQ(repl.execute<int32_t>(R"((17 as i64) as i64)"), 17);

  EXPECT_EQ(repl.execute<uint64_t>(R"((17 as u64) as u64)"), 17);
}

}  // namespace
}  // namespace semantic_analysis
