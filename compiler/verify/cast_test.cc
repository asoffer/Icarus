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
  auto const *expr = mod.Append<ast::Expression>("3 as f64");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::F64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Cast, NonConstant) {
  test::TestModule mod;
  mod.AppendCode("n := 3");
  auto const *expr = mod.Append<ast::Expression>("n as f64");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::F64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Cast, Error) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>("3.0 as i8");
  auto const *qt   = mod.context().qual_type(expr);
  // Continues assuming the type is correct
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::I8)));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-cast")));
}

TEST(Cast, InvalidType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>("3.0 as true");
  auto const *qt   = mod.context().qual_type(expr);
  // Continues assuming the type is correct
  ASSERT_THAT(qt, Pointee(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

// TODO: Cover all possible cast.

}  // namespace
}  // namespace compiler
