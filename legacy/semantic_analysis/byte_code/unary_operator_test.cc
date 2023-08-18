#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(UnaryOperator, Pointers) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<core::Type>("*bool"),
            core::PointerType(GlobalTypeSystem, Bool));

  EXPECT_EQ(repl.execute<core::Type>("*i32"),
            core::PointerType(GlobalTypeSystem, I(32)));

  EXPECT_EQ(repl.execute<core::Type>("***i32"),
            core::PointerType(
                GlobalTypeSystem,
                core::PointerType(GlobalTypeSystem,
                                  core::PointerType(GlobalTypeSystem, I(32)))));
  EXPECT_EQ(repl.execute<core::Type>("[*]bool"),
            BufferPointerType(GlobalTypeSystem, Bool));

  EXPECT_EQ(repl.execute<core::Type>("[*]i32"),
            BufferPointerType(GlobalTypeSystem, I(32)));

  EXPECT_EQ(repl.execute<core::Type>("[*]*[*]i32"),
            BufferPointerType(
                GlobalTypeSystem,
                core::PointerType(GlobalTypeSystem,
                                  BufferPointerType(GlobalTypeSystem, I(32)))));
}

TEST(UnaryOperator, Negation) {
  test::Repl repl;
  EXPECT_EQ(repl.execute<double>("-3.14"), -3.14);
  EXPECT_EQ(repl.execute<core::Integer>("-1234"), -1234);
  EXPECT_EQ(repl.execute<core::Integer>("-(-1234)"), 1234);
}

TEST(UnaryOperator, Not) {
  test::Repl repl;
  EXPECT_FALSE(repl.execute<bool>("not true"));
  EXPECT_TRUE(repl.execute<bool>("not false"));
}

}  // namespace
}  // namespace semantic_analysis
