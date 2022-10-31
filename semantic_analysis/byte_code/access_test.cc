#include "gtest/gtest.h"
#include "ir/value/module_id.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Access, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<uint64_t>(R"("abc".length)"), 3);
}

}  // namespace
}  // namespace semantic_analysis


