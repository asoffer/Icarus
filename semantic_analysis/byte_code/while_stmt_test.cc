#include "gtest/gtest.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

TEST(WhileStmt, Computation) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  (limit: i64) -> i64 {
    n: i64
    while (n < limit) {
      n = 2 * n + 1
    }
    return n
  })");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{4}}, result);
  EXPECT_EQ(result, 7);
  vm::Execute(f, repl.state(), {int64_t{8}}, result);
  EXPECT_EQ(result, 15);
  vm::Execute(f, repl.state(), {int64_t{9}}, result);
  EXPECT_EQ(result, 15);
}

}  // namespace
}  // namespace semantic_analysis
