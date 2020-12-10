#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string expr;
  ir::Value expected;
};

using CastTest = testing::TestWithParam<TestCase>;
TEST_P(CastTest, Cast) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(R"(
  E ::= enum { A ::= 1 as u64 \\ B ::= 2 as u64 \\ C ::= 3 as u64 }
  F ::= flags { A ::= 1 as u64 \\ B ::= 2 as u64 \\ C ::= 4 as u64 }
  )");
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.context().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto t = qt->type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(
    All, CastTest,
    testing::ValuesIn(
        {TestCase{.expr = R"(3 as u64)", .expected = ir::Value(uint64_t{3})},
         TestCase{.expr = R"(3 as i64)", .expected = ir::Value(int64_t{3})},
         TestCase{.expr = R"(3 as f64)", .expected = ir::Value(double{3})},
         TestCase{.expr = R"(3 as i16)", .expected = ir::Value(int16_t{3})},
         TestCase{.expr     = R"(null as *i64)",
                  .expected = ir::Value(ir::Addr::Null())},
         TestCase{.expr     = R"(null as [*]i64)",
                  .expected = ir::Value(ir::Addr::Null())},
         TestCase{.expr = R"(E.A as i64)", .expected = ir::Value(int64_t{1})},
         TestCase{.expr     = R"((F.A | F.B) as u64)",
                  .expected = ir::Value(uint64_t{3})}}));

// TODO: Test casting from an integer into the enum/flags

}  // namespace
}  // namespace compiler
