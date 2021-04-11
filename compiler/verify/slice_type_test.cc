#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(SliceType, Correct) {
  test::TestModule mod;

  auto const *expr = mod.Append<ast::Expression>(R"([]i64)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(SliceType, NonConstantType) {
  test::TestModule mod;

  mod.AppendCode(R"(T := i64)");
  auto const *expr = mod.Append<ast::Expression>(R"([]T)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));

  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(SliceType, NonTypeData) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([]3)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "slice-data-type-not-a-type")));
}

}  // namespace
}  // namespace compiler
