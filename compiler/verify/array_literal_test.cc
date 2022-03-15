#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(ArrayLiteral, EmptyArray) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::EmptyArray)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, OneElement) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([0])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::Integer))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([0 as i64])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(1, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([true])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(1, type::Bool))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([[true]])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::Arr(1, type::Bool)))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([[]])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::EmptyArray))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(ArrayLiteral, MultipleMatchingElements) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([0, 0])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(2, type::Integer))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([true, false])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(2, type::Bool))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([[true], [false]])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(2, type::Arr(1, type::Bool)))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"([[[]], [[]], [[]]])");
    auto const *expr = mod.get<ast::Expression>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(3, type::Arr(1, type::EmptyArray)))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(ArrayLiteral, ElementTypeMismatch) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([0 as i64, 0.0])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "inconsistent-array-element-type")));
}

}  // namespace
}  // namespace compiler
