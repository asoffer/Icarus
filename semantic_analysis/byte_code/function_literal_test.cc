#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(FunctionLiteral, NoParameters) {
  test::Repl repl;

  IrFunction const& f =
      *repl.execute<IrFunction const*>("() -> bool { return true }");
  bool result;
  jasmin::Execute(f, {}, result);
  EXPECT_TRUE(result);
}

TEST(FunctionLiteral, OneParameter) {
  test::Repl repl;

  IrFunction const& f =
      *repl.execute<IrFunction const*>("(n: i64) -> i64 { return -n }");
  int64_t result;

  jasmin::Execute(f, {int64_t{3}}, result);
  EXPECT_EQ(result, -3);

  jasmin::Execute(f, {int64_t{0}}, result);
  EXPECT_EQ(result, 0);

  jasmin::Execute(f, {int64_t{-5}}, result);
  EXPECT_EQ(result, 5);
}

}  // namespace
}  // namespace semantic_analysis
