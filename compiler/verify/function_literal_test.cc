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

TEST(FunctionLiteral, Trivial) {
  test::TestModule mod;
  auto qts =
      mod.context().qual_types(mod.Append<ast::Expression>(R"(() -> () {})"));
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnType) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"(() -> i64 { return 3 })"));
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, InvalidReturnType) {
  test::TestModule mod;
  auto qts =
      mod.context().qual_types(mod.Append<ast::Expression>(R"(() -> 3 {})"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "returning-non-type")));
}

TEST(FunctionLiteral, MultipleValidReturnTypes) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"(() -> (i64, bool) { return 3, true })"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturn) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool) -> () {})"));
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Param("b", type::QualType::NonConstant(type::Bool))}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnType) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool) -> i64 { return 3 })"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturn) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool, n: i64) -> () {})"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::I64))},
                  {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnType) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"((b: bool, n: i64) -> i64 { return 3 })"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::I64))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, ReturnTypeButNoReturnStatement) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"(() -> i64 {})"));
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "returning-wrong-number")));
}

TEST(FunctionLiteral, ReturnTypeWithIncorrectReturnStatement) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"(() -> (i64, bool, i64) { return true, 3, 4 })"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool, type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "return-type-mismatch"),
                                   Pair("type-error", "return-type-mismatch")));
}

TEST(FunctionLiteral, ReturnTypeAllowsCasting) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"((p: [*]i64) -> *i64 { return p })"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Func(
                       {core::Param("p", type::QualType::NonConstant(
                                             type::BufPtr(type::I64)))},
                       {type::Ptr(type::I64)}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, TrivialInferred) {
  test::TestModule mod;
  auto qts =
      mod.context().qual_types(mod.Append<ast::Expression>(R"(() -> {})"));
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnTypeInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"(() -> { return 3 as i64 })"));
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleValidReturnTypesInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"(() -> { return 3 as i64, true })"));
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturnInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool) -> {})"));
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Param("b", type::QualType::NonConstant(type::Bool))}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnTypeInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool) -> { return 3 as i64 })"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturnInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((b: bool, n: i64) -> {})"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::I64))},
                  {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnTypeInferred) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(
      R"((b: bool, n: i64) -> { return 3 as i64 })"));
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::I64))},
                  {type::I64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, ConstantParameter) {
  test::TestModule mod;
  auto qts = mod.context().qual_types(
      mod.Append<ast::Expression>(R"((n :: i64) -> () {})"));
  EXPECT_GE(qts[0].quals(), type::Quals::Const());
  EXPECT_TRUE(qts[0].type().is<type::Generic<type::Function>>());
}

// TODO: Add tests that verify multiple return values get joined correctly.

}  // namespace
}  // namespace compiler
