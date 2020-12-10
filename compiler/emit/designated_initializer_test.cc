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

using DesignatedInitializerTest = testing::TestWithParam<TestCase>;
TEST_P(DesignatedInitializerTest, DesignatedInitializer) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(R"(
  Int ::= struct { n := 3 }
  Pair ::= struct { a: i64 \\ b: bool }
  Wrap ::= struct (T ::= i64) { x: T }

  f ::= () -> (i64, bool) { return 3, true }
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

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    All, DesignatedInitializerTest,
    testing::ValuesIn({
        TestCase{.expr = R"(Int.{}.n)", .expected = ir::Value(int64_t{3})},
        TestCase{.expr = R"(Int.{n = 4}.n)", .expected = ir::Value(int64_t{4})},
        TestCase{.expr = R"(Pair.{}.a)", .expected = ir::Value(int64_t{0})},
        TestCase{.expr = R"(Pair.{}.b)", .expected = ir::Value(false)},
        TestCase{.expr     = R"(Pair.{a = 3}.a)",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr = R"(Pair.{a = 3}.b)", .expected = ir::Value(false)},
        TestCase{.expr     = R"(Pair.{b = true}.a)",
                 .expected = ir::Value(int64_t{0})},
        TestCase{.expr = R"(Pair.{b = true}.b)", .expected = ir::Value(true)},
        TestCase{.expr     = R"(Pair.{a = 3 \\ b = true}.a)",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"(Pair.{a = 3 \\ b = true}.b)",
                 .expected = ir::Value(true)},
        TestCase{.expr     = R"(Wrap(i64).{}.x)",
                 .expected = ir::Value(int64_t{0})},
        TestCase{.expr     = R"(Wrap(i64).{x = 3}.x)",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr = R"(Wrap(bool).{}.x)", .expected = ir::Value(false)},
        TestCase{.expr     = R"(Wrap(bool).{x = true}.x)",
                 .expected = ir::Value(true)},
        TestCase{.expr     = R"(Wrap(f64).{}.x)",
                 .expected = ir::Value(double{0})},
        TestCase{.expr     = R"(Wrap(f64).{x = 3.1}.x)",
                 .expected = ir::Value(3.1)},

        // TODO: Enable these tests once you allow simultaneous assignments.
        // TestCase{.expr     = R"(Pair.{ (a, b) = 'f }.a)",
        //          .expected = ir::Value(int64_t{3})},
        // TestCase{.expr     = R"(Pair.{ (a, b) = 'f }.b)",
        //          .expected = ir::Value(true)},
    }));

}  // namespace
}  // namespace compiler
