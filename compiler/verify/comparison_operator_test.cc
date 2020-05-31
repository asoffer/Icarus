#include "absl/strings/str_format.h"
#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(ComparisonOperator, ConstantSuccess) {
  test::TestModule mod;
  mod.AppendCode(
      R"(x ::= 1
         y ::= 2
      )");
  auto const *expr =
      mod.Append<ast::ComparisonOperator>("x < y <= 3 == x != y >= 3 > x");
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, NonConstantSuccess) {
  test::TestModule mod;
  mod.AppendCode(
      R"(x := 1
         y := 2
      )");
  auto const *expr =
      mod.Append<ast::ComparisonOperator>("x < y <= 3 == x != y >= 3 > x");
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, Pointer) {
  test::TestModule mod;
  mod.AppendCode(
      R"(x := 1
         p := &x
      )");
  auto const *expr = mod.Append<ast::ComparisonOperator>("null != p == &x");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, UnorderedPointers) {
  test::TestModule mod;
  mod.AppendCode(
      R"(x := 1
         p := &x
      )");
  auto const *expr = mod.Append<ast::ComparisonOperator>("p < &x");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "comparing-incomparables")));
}

TEST(ComparisonOperator, BufferPointerOrder) {
  test::TestModule mod;
  mod.AppendCode(
      R"(p: [*]int64
      )");
  auto const *expr = mod.Append<ast::ComparisonOperator>("p < p == p >= p");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

using OperatorOverload = testing::TestWithParam<char const *>;
TEST_P(OperatorOverload, Overloads) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(S ::= struct {}
         (%s) ::= (lhs: S, rhs: S) -> bool { return true }
      )",
      GetParam()));
  auto const *expr = mod.Append<ast::ComparisonOperator>(
      absl::StrFormat("S.{} %s S.{}", GetParam()));
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(OperatorOverload, MissingOverloads) {
  test::TestModule mod;
  mod.AppendCode(
      R"(S ::= struct {}
      )");
  auto const *expr = mod.Append<ast::ComparisonOperator>(
      absl::StrFormat("S.{} %s S.{}", GetParam()));
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-comparison-operator-overload")));
}

INSTANTIATE_TEST_SUITE_P(All, OperatorOverload,
                         testing::ValuesIn({"<", "<=", "==", "!=", ">=", ">"}));

TEST(ComparisonOperator, PriorError) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::ComparisonOperator>("(3 + true) < 3");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}

// TODO: Test where we have an overload but it returns the wrong type.

}  // namespace
}  // namespace compiler
