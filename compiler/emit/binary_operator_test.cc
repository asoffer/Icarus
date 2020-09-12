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

using BinaryOperatorTest = testing::TestWithParam<TestCase>;
TEST_P(BinaryOperatorTest, BinaryOperator) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(R"(
  Color ::= flags {
    RED   ::= 1 as nat64 
    GREEN ::= 2 as nat64
    BLUE  ::= 4 as nat64
  }
  )");
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr) << "No QualType for " << e->DebugString();
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
INSTANTIATE_TEST_SUITE_P(All, BinaryOperatorTest,
                         testing::ValuesIn({
                             // Boolean constant arguments to `|`
                             TestCase{
                                 .expr     = R"(true | true)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(true | false)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(false| true)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(false | false)",
                                 .expected = ir::Value(false),
                             },
                             // Boolean non-constant argumenst to `|`
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a | b))(true, true)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a | b))(true, false)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a | b))(false, true)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a | b))(false, false)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             // TODO: Short-circuiting for boolean `|`
                             TestCase{
                                 .expr     = R"(Color.RED | Color.BLUE)",
                                 .expected = ir::Value(ir::FlagsVal(5)),
                             },
                             TestCase{
                                 .expr     = R"(
                                 (() -> Color {
                                    f := Color.RED
                                    g := Color.BLUE
                                    return f | g
                                 })()
                                 )",
                                 .expected = ir::Value(ir::FlagsVal(5)),
                             },
                             // Boolean constant arguments to `&`
                             TestCase{
                                 .expr     = R"(true & true)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(true & false)",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(false& true)",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(false & false)",
                                 .expected = ir::Value(false),
                             },
                             // Boolean non-constant argumenst to `&`
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a & b))(true, true)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a & b))(true, false)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a & b))(false, true)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a & b))(false, false)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             // TODO: Short-circuiting for boolean `&`
                             TestCase{
                                 .expr     = R"((Color.RED | Color.BLUE) & (Color.BLUE | Color.GREEN))",
                                 .expected = ir::Value(ir::FlagsVal(4)),
                             },
                             TestCase{
                                 .expr     = R"(
                                 (() -> Color {
                                    f := Color.RED | Color.BLUE
                                    g := Color.BLUE | Color.GREEN
                                    return f & g
                                 })()
                                 )",
                                 .expected = ir::Value(ir::FlagsVal(4)),
                             },
                              // Boolean constant arguments to `^`
                             TestCase{
                                 .expr     = R"(true ^ true)",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(true ^ false)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(false^ true)",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(false ^ false)",
                                 .expected = ir::Value(false),
                             },
                             // Boolean non-constant argumenst to `^`
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a ^ b))(true, true)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a ^ b))(true, false)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a ^ b))(false, true)
                                 )",
                                 .expected = ir::Value(true),
                             },
                             TestCase{
                                 .expr     = R"(
                                 ((a: bool, b: bool) => (a ^ b))(false, false)
                                 )",
                                 .expected = ir::Value(false),
                             },
                             TestCase{
                                 .expr     = R"((Color.RED | Color.BLUE) ^ (Color.BLUE | Color.GREEN))",
                                 .expected = ir::Value(ir::FlagsVal(3)),
                             },
                             TestCase{
                                 .expr     = R"(
                                 (() -> Color {
                                    f := Color.RED | Color.BLUE
                                    g := Color.BLUE | Color.GREEN
                                    return f ^ g
                                 })()
                                 )",
                                 .expected = ir::Value(ir::FlagsVal(3)),
                             },
                        }));

}  // namespace
}  // namespace compiler
