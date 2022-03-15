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
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("() -> ()");
  auto const *f = mod.get<ast::FunctionType>();
  auto qts      = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithoutDeclaration) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("(i64, bool) -> (f32, f64)");
  auto const *f = mod.get<ast::FunctionType>();
  auto qts      = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionType, SuccessWithDeclaration) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("(n: i64, b: bool) -> (f32, f64)");
  auto const *f = mod.get<ast::FunctionType>();
  auto qts      = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(FunctionType, NonType) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module("(3, b: bool) -> (f32, 4)");
  auto const *f = mod.get<ast::FunctionType>();
  auto qts      = mod.context().qual_types(f);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-type-function-input"),
                           Pair("type-error", "non-type-function-output")));
}

}  // namespace
}  // namespace compiler
