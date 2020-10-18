#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/overload_set.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(FunctionType, Empty) {
  test::TestModule mod;
  auto const *f  = mod.Append<ast::FunctionType>("() -> ()");
  auto const *qt = mod.data().qual_type(f);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithoutDeclaration) {
  test::TestModule mod;
  auto const *f =
      mod.Append<ast::FunctionType>("(int64, bool) -> (float32, float64)");
  auto const *qt = mod.data().qual_type(f);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithDeclaration) {
  test::TestModule mod;
  auto const *f = mod.Append<ast::FunctionType>(
      "(n: int64, b: bool) -> (float32, float64)");
  auto const *qt = mod.data().qual_type(f);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, NonType) {
  test::TestModule mod;
  auto const *f = mod.Append<ast::FunctionType>("(3, b: bool) -> (float32, 4)");
  auto const *qt = mod.data().qual_type(f);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-type-function-input"),
                           Pair("type-error", "non-type-function-output")));
}

}  // namespace
}  // namespace compiler
