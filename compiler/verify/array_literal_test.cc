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
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::EmptyArray));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, OneElement) {
  test::TestModule mod;
  {
    auto const *expr = mod.Append<ast::Expression>(R"([0])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(1, type::I64)));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([true])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(1, type::Bool)));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[true]])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt,
              type::QualType::Constant(type::Arr(1, type::Arr(1, type::Bool))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[]])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(1, type::EmptyArray)));
  }
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, MultipleMatchingElements) {
  test::TestModule mod;
  {
    auto const *expr = mod.Append<ast::Expression>(R"([0, 0])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(2, type::I64)));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([true, false])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(2, type::Bool)));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[true], [false]])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt,
              type::QualType::Constant(type::Arr(2, type::Arr(1, type::Bool))));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([[[]], [[]], [[]]])");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(
                       type::Arr(3, type::Arr(1, type::EmptyArray))));
  }
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayLiteral, ElementTypeMismatch) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([0, 0.0])");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "inconsistent-array-element-type")));
}

}  // namespace
}  // namespace compiler
