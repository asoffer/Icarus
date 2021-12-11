#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

// TODO: Check that function body verification is scheduled.

TEST(ShortFunctionLiteral, OneValidReturnType) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>("() => 3 as i64"));
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, DISABLED_MultipleValidReturnTypes) {
  test::TestModule mod;
  auto qts =
      mod.context().qual_types(mod.Append<ast::Expression>("() => (3 as i64, true)"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, OneParameterOneReturnType) {
  test::TestModule mod;
  auto qts =
      mod.context().qual_types(mod.Append<ast::Expression>("(b: bool) => 3 as i64"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, MultipleParametersOneReturnType) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"((b: bool, n: i64) => 3 as i64)"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::I64))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, ConstantParameter) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((n :: i64) => n)"));
  EXPECT_GE(qts[0].quals(), type::Quals::Const());
  EXPECT_TRUE(qts[0].type().is<type::Generic<type::Function>>());
}

}  // namespace
}  // namespace compiler
