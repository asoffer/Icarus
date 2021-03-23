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

using ArrayLiteralTest = testing::TestWithParam<TestCase>;
TEST_P(ArrayLiteralTest, ArrayLiteral) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e = mod.Append<ast::Expression>(expr);
  auto t        = mod.context().qual_types(e)[0].type();
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
    All, ArrayLiteralTest,
    testing::ValuesIn({
        TestCase{.expr     = R"((() -> i64 { 'debug_ir \\ return [3][0] })())",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"((() -> i64 { return [1, 4, 9][2] })())",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr = R"((() -> f64 { return [1.0, 4.4, 9.9][1] })())",
                 .expected = ir::Value(4.4)},
        TestCase{.expr     = R"((() -> i64 {
                                   a := [3]
                                   return a[0]
                                 })()
                                 )",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"((() -> i64 {
                                   a := [1, 4, 9]
                                   return a[2]
                                 })()
                                 )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"((() -> f64 {
                                   a := [1.0, 4.4, 9.9]
                                   return a[1]
                                 })()
                                 )",
                 .expected = ir::Value(4.4)},

        TestCase{.expr     = R"((() -> i64 {
                                   a := [3]
                                   return a[0]
                                 })()
                                 )",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"((() -> i64 {
                                   a := [1, 4, 9]
                                   return a[2]
                                 })()
                                 )",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                 .expected = ir::Value(4.4)},
        TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := copy [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                 .expected = ir::Value(4.4)},
        TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a: [3; f64]
                                   a = [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                 .expected = ir::Value(4.4)},
        TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := move [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                 .expected = ir::Value(4.4)},
    }));

}  // namespace
}  // namespace compiler
