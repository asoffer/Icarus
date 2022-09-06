#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;

TEST(Terminal, Evaluation) {
  test::Repl repl;
  EXPECT_THAT(repl.execute("true"), EvaluatesTo(true));
  EXPECT_THAT(repl.execute("false"), EvaluatesTo(false));
}

}  // namespace
}  // namespace semantic_analysis

