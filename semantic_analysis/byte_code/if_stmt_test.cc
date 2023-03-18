#include "gtest/gtest.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

TEST(IfStmt, DISABLED_Computation) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
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
  vm::ExecutionState state{table, repl.type_system()};

  vm::Execute(f, state, {true}, result);
  EXPECT_EQ(result, 3);

  vm::Execute(f, state, {false}, result);
  EXPECT_EQ(result, 4);
}

}  // namespace
}  // namespace semantic_analysis
