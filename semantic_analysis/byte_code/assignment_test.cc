#include "gtest/gtest.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

TEST(Assignment, SingleValue) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  () -> i64 {
    n: i64
    n = 4 as i64
    return n
  })");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{4}}, result);
  EXPECT_EQ(result, 4);
}

TEST(Assignment, WithImplicitCast) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  () -> i64 {
    n: i64
    n = 4
    return n
  })");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{4}}, result);
  EXPECT_EQ(result, 4);
}

TEST(Assignment, MultipleValues) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  (x: i64, y: i64) -> i64 {
    m: i64
    n: i64
    (m, n) = (x, y)
    return 10 * m + n
  })");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{4}, int64_t{5}}, result);
  EXPECT_EQ(result, 45);

  vm::Execute(f, repl.state(), {int64_t{5}, int64_t{4}}, result);
  EXPECT_EQ(result, 54);
}

TEST(Assignment, Intertwined) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  (x: i64, y: i64) -> i64 {
    m := x
    n := y
    (m, n) = (n, m)
    return 10 * m + n
  })");
  int64_t result;
  vm::Execute(f, repl.state(), {int64_t{4}, int64_t{5}}, result);
  EXPECT_EQ(result, 54);
}

}  // namespace
}  // namespace semantic_analysis
