#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;

TEST(Terminal, Evaluation) {
  test::Repl repl;

  EXPECT_THAT(repl.execute("true"), EvaluatesTo(true));
  EXPECT_THAT(repl.execute("false"), EvaluatesTo(false));
  EXPECT_THAT(repl.execute("bool"), EvaluatesTo(Bool));
  EXPECT_THAT(repl.execute("char"), EvaluatesTo(Char));
  EXPECT_THAT(repl.execute("i8"), EvaluatesTo(I(8)));
  EXPECT_THAT(repl.execute("i16"), EvaluatesTo(I(16)));
  EXPECT_THAT(repl.execute("i32"), EvaluatesTo(I(32)));
  EXPECT_THAT(repl.execute("i64"), EvaluatesTo(I(64)));
  EXPECT_THAT(repl.execute("u8"), EvaluatesTo(U(8)));
  EXPECT_THAT(repl.execute("u16"), EvaluatesTo(U(16)));
  EXPECT_THAT(repl.execute("u32"), EvaluatesTo(U(32)));
  EXPECT_THAT(repl.execute("u64"), EvaluatesTo(U(64)));
  EXPECT_THAT(repl.execute("1.25"), EvaluatesTo(1.25));
}

}  // namespace
}  // namespace semantic_analysis
