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

using IdentifierTest = testing::TestWithParam<TestCase>;
TEST_P(IdentifierTest, Identifier) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto const *t = qt->type();
  ASSERT_NE(t, nullptr);
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
    All, IdentifierTest,
    testing::ValuesIn({TestCase{.expr     = R"((() -> int64 {
                                                       a ::= 1
                                                       return a
                                                     })()
                                                     )",
                                .expected = ir::Value(int64_t{1})},
                       TestCase{.expr     = R"(((n :: int64) -> int64 {
                                                       return n
                                                     })(2)
                                                     )",
                                .expected = ir::Value(int64_t{2})},
                       TestCase{.expr     = R"((() -> int64 {
                                                       a := 3
                                                       return a
                                                     })()
                                                     )",
                                .expected = ir::Value(int64_t{3})},
                       TestCase{.expr     = R"(((n: int64) -> int64 {
                                                       return n
                                                     })(4)
                                                     )",
                                .expected = ir::Value(int64_t{4})},
                       TestCase{.expr     = R"((() -> int64 {
                                                       f ::= () -> () {}
                                                       f ::= (n: int64) -> () {}
                                                       f ::= (b: bool) -> () {}
                                                       return f
                                                     })()
                                                     )",
                                .expected = ir::Value(int64_t{4})}}));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
