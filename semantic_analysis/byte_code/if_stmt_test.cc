#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(IfStmt, DISABLED_Computation) {
  test::Repl repl;

  IrFunction const& f =
      *repl.execute<IrFunction const*>(R"(
  (b: bool) -> i64 {
    n: i64
    if (b) {
      n = 3
    } else {
      n = 4
    }
    return n
  })");
  int64_t result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {true}, result);
  EXPECT_EQ(result, 3);

  jasmin::Execute(f, state, {false}, result);
  EXPECT_EQ(result, 4);
}

}  // namespace
}  // namespace semantic_analysis
