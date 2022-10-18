#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Identifier, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<bool>(R"((() -> bool {
    b0: bool
    return b0
  })())"),
            false);
  EXPECT_EQ(repl.execute<int64_t>(R"((() -> i64 {
    n0: i64
    return n0
  })())"),
            int64_t{0});
}

}  // namespace
}  // namespace semantic_analysis
