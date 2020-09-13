#include "absl/strings/str_format.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string op;
  std::string type;
};

struct TestData {
  std::string lhs;
  std::string rhs;
  std::string type;
  ir::Value expected;
};

constexpr char const *kCommonDefinitions = R"(
  Color ::= flags {
    RED   ::= 1 as nat64 
    GREEN ::= 2 as nat64
    BLUE  ::= 4 as nat64
  }
)";

using BinaryOperatorTest =
    testing::TestWithParam<std::tuple<TestCase, TestData>>;
TEST_P(BinaryOperatorTest, Constants) {
  auto const &[test_case, test_data] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(kCommonDefinitions);
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"((%s) %s (%s))", test_data.lhs, test_case.op, test_data.rhs));
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr) << "No QualType for " << e->DebugString();
  auto const *t = qt->type();
  ASSERT_NE(t, nullptr);
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, test_data.expected);
}

TEST_P(BinaryOperatorTest, NonConstants) {
  auto const &[ test_case, test_data] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(kCommonDefinitions);
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"(
      (() -> %s {
        lhs := %s
        rhs := %s
        return lhs %s rhs
      })()
      )",
      test_case.type, test_data.lhs, test_data.rhs, test_case.op));
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr) << "No QualType for " << e->DebugString();
  auto const *t = qt->type();
  ASSERT_NE(t, nullptr);
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, test_data.expected);
}


// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    BooleanOr, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "|", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(true)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(true)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(true)},
        })));

INSTANTIATE_TEST_SUITE_P(
    BooleanAnd, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "&", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(true)},
        })));

INSTANTIATE_TEST_SUITE_P(
    BooleanXor, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "^", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(true)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(true)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(false)},
        })));

// TODO: Short-circuiting test for boolean `|` and `&`.

INSTANTIATE_TEST_SUITE_P(
    FlagsOr, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "|", .type = "Color"}}),
                     testing::ValuesIn({TestData{
                         .lhs      = "Color.RED",
                         .rhs      = "Color.BLUE",
                         .expected = ir::Value(ir::FlagsVal(5))}})));

INSTANTIATE_TEST_SUITE_P(
    FlagsAnd, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "&", .type = "Color"}}),
                     testing::ValuesIn({TestData{
                         .lhs      = "Color.RED | Color.GREEN",
                         .rhs      = "Color.BLUE | Color.GREEN",
                         .expected = ir::Value(ir::FlagsVal(2))}})));

INSTANTIATE_TEST_SUITE_P(
    FlagsXor, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "^", .type = "Color"}}),
                     testing::ValuesIn({TestData{
                         .lhs      = "Color.RED | Color.GREEN",
                         .rhs      = "Color.BLUE | Color.GREEN",
                         .expected = ir::Value(ir::FlagsVal(5))}})));

}  // namespace
}  // namespace compiler
