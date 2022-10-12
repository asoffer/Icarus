#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Terminal, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<bool>("true"), true);
  EXPECT_EQ(repl.execute<bool>("false"), false);
  EXPECT_EQ(repl.execute<core::Type>("bool"), Bool);
  EXPECT_EQ(repl.execute<core::Type>("char"), Char);
  EXPECT_EQ(repl.execute<core::Type>("i8"), I(8));
  EXPECT_EQ(repl.execute<core::Type>("i16"), I(16));
  EXPECT_EQ(repl.execute<core::Type>("i32"), I(32));
  EXPECT_EQ(repl.execute<core::Type>("i64"), I(64));
  EXPECT_EQ(repl.execute<core::Type>("u8"), U(8));
  EXPECT_EQ(repl.execute<core::Type>("u16"), U(16));
  EXPECT_EQ(repl.execute<core::Type>("u32"), U(32));
  EXPECT_EQ(repl.execute<core::Type>("u64"), U(64));
  EXPECT_EQ(repl.execute<double>("1.25"), 1.25);
}

}  // namespace
}  // namespace semantic_analysis
