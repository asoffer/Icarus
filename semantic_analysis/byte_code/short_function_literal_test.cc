#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

TEST(FunctionLiteral, NoParameters) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>("() => true");
  bool result;
  vm::Execute(f, repl.state(), {}, result);
  EXPECT_TRUE(result);
}

TEST(FunctionLiteral, OneParameter) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>("(n: i64) => -n");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{3}}, result);
  EXPECT_EQ(result, -3);

  vm::Execute(f, repl.state(), {int64_t{0}}, result);
  EXPECT_EQ(result, 0);

  vm::Execute(f, repl.state(), {int64_t{-5}}, result);
  EXPECT_EQ(result, 5);
}

}  // namespace
}  // namespace semantic_analysis
