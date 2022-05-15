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
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(x ::= 1 as i64
         y ::= 2 as i64

         x < y <= 3 == x != y >= 3 > x
      )");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, NonConstantSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(x := 1 as i64
         y := 2 as i64

         x < y <= 3 == x != y >= 3 > x
      )");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, Pointer) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(x: i64
         p := &x

         null != p == &x
      )");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ComparisonOperator, UnorderedPointers) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(x := 1 as i64
         p := &x

         p < &x
      )");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "comparing-incomparables")));
}

TEST(ComparisonOperator, BufferPointerOrder) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  p: [*]i64
  p < p == p >= p
  )");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

// using OperatorOverload = testing::TestWithParam<char const *>;
// TEST_P(OperatorOverload, Overloads) {
//   test::CompilerInfrastructure infra;
//   auto &mod        = infra.add_module(absl::StrFormat(
//       R"(S ::= struct {}
//          (%s) ::= (lhs: S, rhs: S) -> bool { return true }
//          S.{} %s S.{}
//       )",
//       GetParam(), GetParam()));
//   auto const *expr = mod.get<ast::ComparisonOperator>();
//   auto qts         = mod.context().qual_types(expr);
//   EXPECT_THAT(qts,
//               UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
//   EXPECT_THAT(infra.diagnostics(), IsEmpty());
// }
// 
// TEST_P(OperatorOverload, MissingOverloads) {
//   test::CompilerInfrastructure infra;
//   auto &mod        = infra.add_module(absl::StrFormat(
//       R"(
//       S ::= struct {}
//       S.{} %s S.{}
//       )",
//       GetParam()));
//   auto const *expr = mod.get<ast::ComparisonOperator>();
//   auto qts         = mod.context().qual_types(expr);
//   EXPECT_THAT(qts,
//               UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
//   EXPECT_THAT(infra.diagnostics(),
//               UnorderedElementsAre(
//                   Pair("type-error", "invalid-comparison-operator-overload")));
// }
// 
// TODO: Support operator overloading for comparisons.
// INSTANTIATE_TEST_SUITE_P(All, OperatorOverload,
//                          testing::ValuesIn({"<", "<=", "==", "!=", ">=",
//                          ">"}));

TEST(ComparisonOperator, PriorError) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module("(3 + true) < 3");
  auto const *expr = mod.get<ast::ComparisonOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "no-matching-binary-operator")));
}

// TODO: Test where we have an overload but it returns the wrong type.

}  // namespace
}  // namespace compiler
