#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(SliceType, Construction) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<core::Type>("[/]bool"), SliceType(GlobalTypeSystem, Bool));

  EXPECT_EQ(repl.execute<core::Type>("[/]char"), SliceType(GlobalTypeSystem, Char));

  EXPECT_EQ(repl.execute<core::Type>("[/][/]bool"),
            SliceType(GlobalTypeSystem, SliceType(GlobalTypeSystem, Bool)));
}

}  // namespace
}  // namespace semantic_analysis
