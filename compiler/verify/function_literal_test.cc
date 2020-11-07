#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::UnorderedElementsAre;

// TODO: Check that function body verification is scheduled.

TEST(FunctionLiteral, Trivial) {
  test::TestModule mod;
  auto const *qt =
      mod.context().qual_type(mod.Append<ast::Expression>(R"(() -> () {})"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnType) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"(() -> int64 { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func({}, {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, InvalidReturnType) {
  test::TestModule mod;
  auto const *qt =
      mod.context().qual_type(mod.Append<ast::Expression>(R"(() -> 3 {})"));
  EXPECT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "returning-non-type")));
}

TEST(FunctionLiteral, MultipleValidReturnTypes) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"(() -> (int64, bool) { return 3, true })"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(
                      type::Func({}, {type::Int64, type::Bool}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturn) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool) -> () {})"));
  EXPECT_THAT(
      qt,
      Pointee(type::QualType::Constant(type::Func(
          {core::Param("b", type::QualType::NonConstant(type::Bool))}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnType) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool) -> int64 { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool))},
                  {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturn) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool, n: int64) -> () {})"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::Int64))},
                  {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnType) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(mod.Append<ast::Expression>(
      R"((b: bool, n: int64) -> int64 { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::Int64))},
                  {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, ReturnTypeButNoReturnStatement) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(mod.Append<ast::Expression>(
      R"(() -> int64 {})"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func({}, {type::Int64}))));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "returning-wrong-number")));
}

TEST(FunctionLiteral, ReturnTypeWithIncorrectReturnStatement) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(mod.Append<ast::Expression>(
      R"(() -> (int64, bool, int64) { return true, 3, 4 })"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(
                      type::Func({}, {type::Int64, type::Bool, type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "return-type-mismatch"),
                                   Pair("type-error", "return-type-mismatch")));
}

TEST(FunctionLiteral, ReturnTypeAllowsCasting) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(mod.Append<ast::Expression>(
      R"((p: [*]int64) -> *int64 { return p })"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Func(
                      {core::Param("p", type::QualType::NonConstant(
                                            type::BufPtr(type::Int64)))},
                      {type::Ptr(type::Int64)}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, TrivialInferred) {
  test::TestModule mod;
  auto const *qt =
      mod.context().qual_type(mod.Append<ast::Expression>(R"(() -> {})"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnTypeInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"(() -> { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func({}, {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleValidReturnTypesInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"(() -> { return 3, true })"));
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(
                      type::Func({}, {type::Int64, type::Bool}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturnInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool) -> {})"));
  EXPECT_THAT(
      qt,
      Pointee(type::QualType::Constant(type::Func(
          {core::Param("b", type::QualType::NonConstant(type::Bool))}, {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnTypeInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool) -> { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool))},
                  {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturnInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((b: bool, n: int64) -> {})"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::Int64))},
                  {}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnTypeInferred) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(mod.Append<ast::Expression>(
      R"((b: bool, n: int64) -> { return 3 })"));
  EXPECT_THAT(qt,
              Pointee(type::QualType::Constant(type::Func(
                  {core::Param("b", type::QualType::NonConstant(type::Bool)),
                   core::Param("n", type::QualType::NonConstant(type::Int64))},
                  {type::Int64}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, ConstantParameter) {
  test::TestModule mod;
  auto const *qt = mod.context().qual_type(
      mod.Append<ast::Expression>(R"((n :: int64) -> () {})"));
  ASSERT_NE(qt, nullptr);
  EXPECT_GE(qt->quals(), type::Quals::Const());
  EXPECT_TRUE(qt->type().is<type::GenericFunction>());
}

// TODO: Add tests that verify multiple return values get joined correctly.

}  // namespace
}  // namespace compiler
