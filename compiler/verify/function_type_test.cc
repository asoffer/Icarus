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
  auto const *f     = mod.Append<ast::FunctionType>("() -> ()");
  auto qts  = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre( type::QualType::Constant(type::Type_)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithoutDeclaration) {
  test::TestModule mod;
  auto const *f = mod.Append<ast::FunctionType>("(i64, bool) -> (f32, f64)");
  auto qts  = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre( type::QualType::Constant(type::Type_)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithDeclaration) {
  test::TestModule mod;
  auto const *f =
      mod.Append<ast::FunctionType>("(n: i64, b: bool) -> (f32, f64)");
  auto qts  = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre( type::QualType::Constant(type::Type_)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(FunctionType, NonType) {
  test::TestModule mod;
  auto const *f     = mod.Append<ast::FunctionType>("(3, b: bool) -> (f32, 4)");
  auto qts  = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre( type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-type-function-input"),
                           Pair("type-error", "non-type-function-output")));
}

}  // namespace
}  // namespace compiler
