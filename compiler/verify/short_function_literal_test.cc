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
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("() => 3 as i64");
  auto const *e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, DISABLED_MultipleValidReturnTypes) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("() => (3 as i64, true)");
  auto const *e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, OneParameterOneReturnType) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("(b: bool) => 3 as i64");
  auto const *e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name = "b", .value = type::QualType::NonConstant(type::Bool)}},
          {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, MultipleParametersOneReturnType) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"((b: bool, n: i64) => 3 as i64)");
  auto const *e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
               .name = "b", .value = type::QualType::NonConstant(type::Bool)},
           core::Parameter<type::QualType>{
               .name = "n", .value = type::QualType::NonConstant(type::I64)}},
          {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ShortFunctionLiteral, ConstantParameter) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"((n :: i64) => n)");
  auto const *e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_GE(qts[0].quals(), type::Quals::Const());
  EXPECT_TRUE(qts[0].type().is<type::Generic<type::Function>>());
}

}  // namespace
}  // namespace compiler
