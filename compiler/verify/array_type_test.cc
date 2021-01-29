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

TEST(ArrayType, Correct) {
  test::TestModule mod;

  {
    auto const *expr = mod.Append<ast::Expression>(R"([3; i64])");
    auto const *qt   = mod.context().qual_type(expr);
    EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));
  }

  {
    auto const *expr = mod.Append<ast::Expression>(R"([2; [3; i64]])");
    auto const *qt   = mod.context().qual_type(expr);
    EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));
  }

  {
    auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3; i64])");
    auto const *qt   = mod.context().qual_type(expr);
    EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));
  }
  {
    auto const *expr = mod.Append<ast::Expression>(R"([1 as i8; i64])");
    auto const *qt   = mod.context().qual_type(expr);
    EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));
  }

  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayType, NonConstantType) {
  test::TestModule mod;

  mod.AppendCode(R"(T := i64)");
  auto const *expr = mod.Append<ast::Expression>(R"([3; T])");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::Type_)));

  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayType, NonTypeElement) {
  test::TestModule mod;

  auto const *expr = mod.Append<ast::Expression>(R"([3; 2])");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "array-data-type-not-a-type")));
}

TEST(ArrayType, NonConstantLength) {
  test::TestModule mod;
  mod.AppendCode(R"(n := 3)");
  auto const *expr = mod.Append<ast::Expression>(R"([3, n, 2; i64])");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::Type_)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ArrayType, NonIntegerLength) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3.0; i64])");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-array-length")));
}

TEST(ArrayType, NonIntegerNonConstant) {
  test::TestModule mod;
  mod.AppendCode(R"(x := 3.0)");
  auto const *expr = mod.Append<ast::Expression>(R"([x; i64])");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::Type_)));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-array-length")));
}

}  // namespace
}  // namespace compiler
