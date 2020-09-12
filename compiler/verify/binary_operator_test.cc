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

using BoolLogicalOperatorEq = testing::TestWithParam<char const *>;
TEST_P(BoolLogicalOperatorEq, Success) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    b: bool
    b %s true
    )",
                                 GetParam()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(BoolLogicalOperatorEq, NonReference) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    b: bool
    !b %s true
    )",
                                 GetParam()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(BoolLogicalOperatorEq, Constant) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    b :: bool
    b %s true
    )",
                                 GetParam()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(BoolLogicalOperatorEq, InvalidLhsType) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    n: int64
    n %s true
    )",
                                 GetParam()));

  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "logical-assignment-needs-bool-or-flags")));
}

TEST_P(BoolLogicalOperatorEq, InvalidRhsType) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    b: bool
    b %s 3
    )",
                                 GetParam()));

  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "logical-assignment-needs-bool-or-flags")));
}
INSTANTIATE_TEST_SUITE_P(All, BoolLogicalOperatorEq,
                         testing::ValuesIn({"^=", "&=", "|="}));

using FlagsLogicalOperatorEq = testing::TestWithParam<char const *>;
TEST_P(FlagsLogicalOperatorEq, Success) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    F ::= flags { A \\ B \\ C }
    f: F
    f %s f
    )",
                                 GetParam()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(FlagsLogicalOperatorEq, NonReference) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    F ::= flags { A \\ B \\ C }
    f: F
    !f %s f
    )",
                                 GetParam()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(FlagsLogicalOperatorEq, Constant) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    F ::= flags { A \\ B \\ C }
    f :: F
    f %s F.A
    )",
                                 GetParam()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(FlagsLogicalOperatorEq, InvalidLhsType) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    F ::= flags { A \\ B \\ C }
    n: int64
    n %s F.A
    )",
                                 GetParam()));

  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "logical-assignment-needs-bool-or-flags")));
}

TEST_P(FlagsLogicalOperatorEq, InvalidRhsType) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(R"(
    F ::= flags { A \\ B \\ C }
    f: F
    f %s 3
    )",
                                 GetParam()));

  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "logical-assignment-needs-bool-or-flags")));
}
INSTANTIATE_TEST_SUITE_P(All, FlagsLogicalOperatorEq,
                         testing::ValuesIn({"^=", "&=", "|="}));

using IntegerArithmeticOperatorEq =
    testing::TestWithParam<std::tuple<char const *, char const *>>;
TEST_P(IntegerArithmeticOperatorEq, Success) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n: %s
         n %s n
      )",
      type, op));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(IntegerArithmeticOperatorEq, NonReference) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n: %s
         // `n + n` is a valid non-reference expression for all integral types.
         (n + n) %s n
      )",
      type, op));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(IntegerArithmeticOperatorEq, Constant) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n :: %s
         n %s n
      )",
      type, op));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(IntegerArithmeticOperatorEq, InvalidLhsType) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n: %s
         b: bool
         b %s n
      )",
      type, op));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}

TEST_P(IntegerArithmeticOperatorEq, InvalidRhsType) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n: %s
         n %s true
      )",
      type, op));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}
INSTANTIATE_TEST_SUITE_P(
    All, IntegerArithmeticOperatorEq,
    testing::Combine(testing::ValuesIn({"int8", "int16", "int32", "int64",
                                        "nat8", "nat16", "nat32", "nat64"}),
                     testing::ValuesIn({"+=", "-=", "*=", "/=", "%="})));

using FloatingPointArithmeticOperatorEq =
    testing::TestWithParam<std::tuple<char const *, char const *>>;
TEST_P(FloatingPointArithmeticOperatorEq, Success) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(n: %s
         n %s n
      )",
      type, op));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(FloatingPointArithmeticOperatorEq, NonReference) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(x: %s
         // `x + x` is a valid non-reference expression for all floating-point types.
         (x + x) %s x
      )",
      type, op));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(FloatingPointArithmeticOperatorEq, Constant) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(x :: %s
         x %s x
      )",
      type, op));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-category-error",
                                "invalid-assignment-lhs-value-category")));
}

TEST_P(FloatingPointArithmeticOperatorEq, InvalidLhsType) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(x: %s
         b: bool
         b %s x
      )",
      type, op));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}

TEST_P(FloatingPointArithmeticOperatorEq, InvalidRhsType) {
  auto [type, op] = GetParam();
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(x: %s
         x %s true
      )",
      type, op));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}
INSTANTIATE_TEST_SUITE_P(
    All, FloatingPointArithmeticOperatorEq,
    testing::Combine(testing::ValuesIn({"float32", "float64"}),
                     testing::ValuesIn({"+=", "-=", "*=", "/="})));

using BinaryOperator =
    testing::TestWithParam<std::tuple<char const *, char const *>>;
TEST_P(BinaryOperator, Success) {
  auto [type, op] = GetParam();
  {
    test::TestModule mod;
    mod.AppendCode("FlagType ::= flags {}");
    mod.AppendCode(absl::StrCat(R"(x: )", type));
    auto const *expr =
        mod.Append<ast::BinaryOperator>(absl::StrFormat("x %s x", op));
    auto const *qt = mod.data().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
    EXPECT_EQ(qt->type(), mod.data().qual_type(expr->lhs())->type());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
  {
    test::TestModule mod;
    mod.AppendCode("FlagType ::= flags {}");
    mod.AppendCode(absl::StrCat(R"(x :: )", type));
    auto const *expr =
        mod.Append<ast::BinaryOperator>(absl::StrFormat("x %s x", op));
    auto const *qt = mod.data().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(qt->quals(), type::Quals::Const());
    EXPECT_EQ(qt->type(), mod.data().qual_type(expr->lhs())->type());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

INSTANTIATE_TEST_SUITE_P(
    Integers, BinaryOperator,
    testing::Combine(testing::ValuesIn({"int8", "int16", "int32", "int64",
                                        "nat8", "nat16", "nat32", "nat64"}),
                     testing::ValuesIn({"+", "-", "*", "/", "%"})));
INSTANTIATE_TEST_SUITE_P(
    FloatingPoint, BinaryOperator,
    testing::Combine(testing::ValuesIn({"float32", "float64"}),
                     testing::ValuesIn({"+", "-", "*", "/"})));
INSTANTIATE_TEST_SUITE_P(LogicalOperator, BinaryOperator,
                         testing::Combine(testing::ValuesIn({"bool",
                                                             "FlagType"}),
                                          testing::ValuesIn({"^", "&", "|"})));

using OperatorOverload = testing::TestWithParam<char const *>;
TEST_P(OperatorOverload, Overloads) {
  test::TestModule mod;
  mod.AppendCode(absl::StrFormat(
      R"(S ::= struct {}
         (%s) ::= (lhs: S, rhs: S) -> int64 { return 0 }
      )",
      GetParam()));
  auto const *expr = mod.Append<ast::BinaryOperator>(
      absl::StrFormat("S.{} %s S.{}", GetParam()));
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST_P(OperatorOverload, MissingOverloads) {
  test::TestModule mod;
  mod.AppendCode(
      R"(S ::= struct {}
      )");
  auto const *expr = mod.Append<ast::BinaryOperator>(
      absl::StrFormat("S.{} %s S.{}", GetParam()));
  auto const *qt = mod.data().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-binary-operator-overload")));
}

INSTANTIATE_TEST_SUITE_P(All, OperatorOverload,
                         testing::ValuesIn({"+", "-", "*", "/", "%", "^", "&",
                                            "|"}));

// TODO: Assignment operator overloading tests.

}  // namespace
}  // namespace compiler
