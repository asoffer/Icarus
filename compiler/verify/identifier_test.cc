#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/overload_set.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(Identifier, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n: int64
  )");
  auto const *id = mod.Append<ast::Identifier>("n");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_NE(qt, nullptr);
  EXPECT_THAT(mod.context().decls(id), SizeIs(1));
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Identifier, Undeclared) {
  test::TestModule mod;
  auto const *id = mod.Append<ast::Identifier>("n");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Identifier, OverloadSetSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f := () => 3
  f ::= (b: bool) => 4
  )");
  auto const *id = mod.Append<ast::Identifier>("f");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::OverloadSet>());
  EXPECT_THAT(mod.context().decls(id), SizeIs(2));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Identifier, NonCallableOverloads) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f := 3
  f ::= (b: bool) => 4
  f := true
  )");
  auto const *id = mod.Append<ast::Identifier>("f");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "shadowing-declaration"),
                           Pair("type-error", "shadowing-declaration"),
                           Pair("type-error", "shadowing-declaration"),
                           Pair("type-error", "non-callable-in-overload-set"),
                           Pair("type-error", "non-callable-in-overload-set")));
}

TEST(Identifier, CyclicDependency) {
  test::TestModule mod;
  mod.AppendCode(R"(
  x ::= y + 1
  y ::= z + 1
  z ::= x + 1
  )");
  ASSERT_EQ(mod.context().qual_type(mod.Append<ast::Identifier>("x")), nullptr);
  ASSERT_EQ(mod.context().qual_type(mod.Append<ast::Identifier>("y")), nullptr);
  ASSERT_EQ(mod.context().qual_type(mod.Append<ast::Identifier>("z")), nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "cyclic-dependency")));
}

}  // namespace
}  // namespace compiler
