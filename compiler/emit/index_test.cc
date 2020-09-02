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
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto const *t = qt->type();
  ASSERT_NE(qt, nullptr);
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
        TestCase{.expr = R"("abc"[0])", .expected = ir::Value(uint8_t{'a'})},
        TestCase{.expr = R"([1, 2, 3][0])", .expected = ir::Value(int64_t{1})},
        TestCase{.expr     = R"((() -> int64 {
        a := [1, 2, 3]
        p: [*]int64 = &a[1]
        return p[1]
        })()
        )",
                 .expected = ir::Value(int64_t{3})},
    }));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
