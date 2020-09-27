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

TEST(Cast, ConstantSuccess) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>("3 as float64");
  auto const *qt   = mod.data().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Float64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Cast, NonConstant) {
  test::TestModule mod;
  mod.AppendCode("n := 3");
  auto const *expr = mod.Append<ast::Expression>("n as float64");
  auto const *qt   = mod.data().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::Float64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Cast, Error) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>("3.0 as int8");
  auto const *qt   = mod.data().qual_type(expr);
  // Continues assuming the type is correct
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Int8)));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-cast")));
}

TEST(Cast, InvalidType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>("3.0 as true");
  auto const *qt   = mod.data().qual_type(expr);
  // Continues assuming the type is correct
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

}  // namespace
}  // namespace compiler
