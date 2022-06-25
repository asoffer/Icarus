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
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> () {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnType) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> i64 { return 3 })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, InvalidReturnType) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> 3 {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "returning-non-type")));
}

TEST(FunctionLiteral, MultipleValidReturnTypes) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> (i64, bool) { return 3, true })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturn) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool) -> () {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name = "b", .value = type::QualType::NonConstant(type::Bool)}},
          {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnType) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool) -> i64 { return 3 })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name = "b", .value = type::QualType::NonConstant(type::Bool)}},
          {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturn) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool, n: i64) -> () {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
               .name = "b", .value = type::QualType::NonConstant(type::Bool)},
           core::Parameter<type::QualType>{
               .name = "n", .value = type::QualType::NonConstant(type::I64)}},
          {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnType) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool, n: i64) -> i64 { return 3 })");
  auto const* e = mod.get<ast::Expression>();
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

TEST(FunctionLiteral, ReturnTypeButNoReturnStatement) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> i64 {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "returning-wrong-number")));
}

TEST(FunctionLiteral, ReturnTypeWithIncorrectReturnStatement) {
  test::CompilerInfrastructure infra;
  auto& mod =
      infra.add_module(R"(() -> (i64, bool, i64) { return true, 3, 4 })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool, type::I64}))));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "return-type-mismatch"),
                                   Pair("type-error", "return-type-mismatch")));
}

TEST(FunctionLiteral, ReturnTypeAllowsCasting) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((p: [*]i64) -> *i64 { return p })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name  = "p",
              .value = type::QualType::NonConstant(type::BufPtr(type::I64))}},
          {type::Ptr(type::I64)}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, TrivialInferred) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType::Constant(type::Func({}, {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneValidReturnTypeInferred) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> { return 3 as i64 })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::Constant(type::Func({}, {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleValidReturnTypesInferred) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(() -> { return 3 as i64, true })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                       type::Func({}, {type::I64, type::Bool}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterNoReturnInferred) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool) -> {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name = "b", .value = type::QualType::NonConstant(type::Bool)}},
          {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, OneParameterOneReturnTypeInferred) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((b: bool) -> { return 3 as i64 })");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
              .name = "b", .value = type::QualType::NonConstant(type::Bool)}},
          {type::I64}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersNoReturnInferred) {
  test::CompilerInfrastructure infra;
  auto& mod = infra.add_module(R"((b: bool, n: i64) -> {})");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(
      qts,
      UnorderedElementsAre(type::QualType::Constant(type::Func(
          {core::Parameter<type::QualType>{
               .name = "b", .value = type::QualType::NonConstant(type::Bool)},
           core::Parameter<type::QualType>{
               .name = "n", .value = type::QualType::NonConstant(type::I64)}},
          {}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionLiteral, MultipleParametersOneReturnTypeInferred) {
  test::CompilerInfrastructure infra;
  auto& mod = infra.add_module(R"((b: bool, n: i64) -> { return 3 as i64 })");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
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

TEST(FunctionLiteral, ConstantParameter) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"((n :: i64) -> () {})");
  auto const* e = mod.get<ast::Expression>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType::Constant(type::Func(
               {core::Parameter<type::QualType>{
                   .name = "n", .value = type::QualType::Constant(type::I64)}},
               {}))));
}

// TODO: Add tests that verify multiple return values get joined correctly.

}  // namespace
}  // namespace compiler
