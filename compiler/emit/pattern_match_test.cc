#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string pattern;
  std::string expr;
  ir::Value expected;
};

using PatternMatchTest = testing::TestWithParam<TestCase>;
TEST_P(PatternMatchTest, Access) {
  auto const &[pattern, expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(pattern);
  auto const *e = mod.Append<ast::Expression>(expr);
  auto t        = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(All, PatternMatchTest,
                         testing::ValuesIn({
                             TestCase{.pattern  = "20 ~ `N",
                                      .expr     = "N",
                                      .expected = ir::Value(int64_t{20})},
                             TestCase{.pattern  = "true ~ `B",
                                      .expr     = "B",
                                      .expected = ir::Value(true)},
                             TestCase{.pattern  = "23 ~ 3 + 4 * `N",
                                      .expr     = "N",
                                      .expected = ir::Value(int64_t{5})},
                         }));

}  // namespace
}  // namespace compiler
