#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Identifier, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<bool>(R"(() -> {
    b0: bool
    return b0
  }())"),
            false);
  EXPECT_EQ(repl.execute<bool>(R"(() -> {
    b1 := true
    return b1
  }())"),
            true);

  EXPECT_EQ(repl.execute<int64_t>(R"(() -> {
    n0: i64
    return n0
  }())"),
            int64_t{0});
}

}  // namespace
}  // namespace semantic_analysis
