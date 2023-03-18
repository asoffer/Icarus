#include "gtest/gtest.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

TEST(Assignment, DISABLED_SingleValue) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  () -> i64 {
    n: i64
    n = 4 as i64
    return n
  })");
  int64_t result;
  data_types::IntegerTable table;
  vm::ExecutionState state{table, repl.type_system()};

  vm::Execute(f, state, {int64_t{4}}, result);
  EXPECT_EQ(result, 4);
}

TEST(Assignment, DISABLED_MultipleValues) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  (x: i64, y: i64) -> i64 {
    m: i64
    n: i64
    (m, n) = (x, y)
    return n
  })");
  int64_t result;
  data_types::IntegerTable table;
  vm::ExecutionState state{table, repl.type_system()};

  vm::Execute(f, state, {int64_t{4}, int64_t{5}}, result);
  EXPECT_EQ(result, 45);

  vm::Execute(f, state, {int64_t{5}, int64_t{4}}, result);
  EXPECT_EQ(result, 54);
}

TEST(Assignment, DISABLED_Intertwined) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  (x: i64, y: i64) -> i64 {
    m := x
    n := y
    (m, n) = (n, m)
    return n
  })");
  int64_t result;
  data_types::IntegerTable table;
  vm::ExecutionState state{table, repl.type_system()};

  vm::Execute(f, state, {int64_t{4}, int64_t{5}}, result);
  EXPECT_EQ(result, 45);
}

}  // namespace
}  // namespace semantic_analysis
