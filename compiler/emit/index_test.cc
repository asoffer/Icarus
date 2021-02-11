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

using IndexTest = testing::TestWithParam<TestCase>;
TEST_P(IndexTest, Index) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(expr);
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
    All, IndexTest,
    testing::ValuesIn({
        TestCase{.expr = R"("abc"[0])", .expected = ir::Value(ir::Char('a'))},
        TestCase{.expr = R"([1, 2, 3][0])", .expected = ir::Value(int64_t{1})},
        TestCase{.expr     = R"((() -> i64 {
        // Reference buffer-pointer indexing.
        a := [1, 2, 3]
        p: [*]i64 = &a[1]
        return p[1]
        })()
        )",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"((() -> i64 {
        // Non-reference buffer-pointer indexing.
        a := [1, 2, 3, 4]
        p: [*]i64 = &a[1]
        return (&(p[1]))[1]
        })()
        )",
                 .expected = ir::Value(int64_t{4})}
    }));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
