#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string description;
  std::string expr;
  ir::Value expected;
};

using CallTest = testing::TestWithParam<TestCase>;
TEST_P(CallTest, Call) {
  auto const &[description, expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(R"(
  identity ::= (x: $x) => x
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
    All, CallTest,
    testing::ValuesIn({
        TestCase{.expr     = R"(bytes(i64)
                             )",
                 .expected = ir::Value(uint64_t{sizeof(int64_t)})},
        TestCase{.expr     = R"(alignment(i64)
                             )",
                 .expected = ir::Value(uint64_t{alignof(int64_t)})},

        // TODO: Test for opaque, foreign.

        TestCase{.expr     = R"((() => 3)()
                             )",
                 .expected = ir::Value(int64_t{3})},

        TestCase{.expr     = R"(((n: i64) => n * n)(3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n: i64) => n * n)(n = 3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)(3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)(n = 3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)()
                             )",
                 .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((n :: i64) => n * n)(3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n :: i64) => n * n)(n = 3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n ::= 2) => n * n)(3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n ::= 2) => n * n)(n = 3)
                             )",
                 .expected = ir::Value(int64_t{9})},
        // TestCase{.expr     = R"(((n ::= 2) => n * n)()
        //                      )",
        //          .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((a: i64, b: i64) => a + 2 * b)(1, 2)
                             )",
                 .expected = ir::Value(int64_t{5})},
        TestCase{.expr = R"(((a: i64, b: i64) => a + 2 * b)(a = 1, b = 2)
                             )",
                 .expected = ir::Value(int64_t{5})},
        TestCase{.expr = R"(((a: i64, b: i64) => a + 2 * b)(b = 1, a = 2)
                             )",
                 .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1, a = 2)
                             )",
                 .expected = ir::Value(int64_t{4})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(a = 2)
                             )",
                 .expected = ir::Value(int64_t{6})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1)
                             )",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(2)
                             )",
                 .expected = ir::Value(int64_t{6})},

        TestCase{.expr     = R"(((a: $a) => a * a)(2)
                             )",
                 .expected = ir::Value(int64_t{4})},
        TestCase{.expr     = R"(((a: $a) => a * a)(2.5)
                             )",
                 .expected = ir::Value(6.25)},

        // TODO: Calling overload sets.
        // TODO: There's something wrong with casting. When that's fixed we can
        // try this with non-zero values. For now this test is at least
        // verifying that we can instantiate both versions without crashing.
        TestCase{.description = "Instantiate the same generic more than once.",
                 .expr        = R"((identity(0) as f64) + identity(0.0)
                             )",
                 .expected    = ir::Value(double{0})},
    }));

}  // namespace
}  // namespace compiler
