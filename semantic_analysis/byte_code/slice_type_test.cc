#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(SliceType, Construction) {
  test::Repl repl;
  auto& type_system = repl.type_system();

  EXPECT_EQ(repl.execute<core::Type>("[/]bool"), SliceType(type_system, Bool));

  EXPECT_EQ(repl.execute<core::Type>("[/]char"), SliceType(type_system, Char));

  EXPECT_EQ(repl.execute<core::Type>("[/][/]bool"),
            SliceType(type_system, SliceType(type_system, Bool)));
}

}  // namespace
}  // namespace semantic_analysis
