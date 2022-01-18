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
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::EmptyArray)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, OneElement) {
  test::TestModule mod;
  {
    auto const *expr = mod.Append<ast::Expression>(R"([0])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::Integer))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([0 as i64])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(1, type::I64))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([true])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(1, type::Bool))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[true]])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::Arr(1, type::Bool)))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[]])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(1, type::EmptyArray))));
  }
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, MultipleMatchingElements) {
  test::TestModule mod;
  {
    auto const *expr = mod.Append<ast::Expression>(R"([0, 0])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(2, type::Integer))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([true, false])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(2, type::Bool))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[true], [false]])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(2, type::Arr(1, type::Bool)))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[[]], [[]], [[]]])");
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(3, type::Arr(1, type::EmptyArray)))));
  }
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, ElementTypeMismatch) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([0 as i64, 0.0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "inconsistent-array-element-type")));
}

}  // namespace
}  // namespace compiler
