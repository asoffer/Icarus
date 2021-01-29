#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/overload_set.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(Identifier, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n: i64
  )");
  auto const *id = mod.Append<ast::Identifier>("n");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_THAT(qt, Pointee(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(mod.context().decls(id), SizeIs(1));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Identifier, Undeclared) {
  test::TestModule mod;
  auto const *id = mod.Append<ast::Identifier>("n");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_THAT(qt, Pointee(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Identifier, UndeclaredDoesNotRepeat) {
  test::TestModule mod;
  mod.AppendCode(R"(
  x := f(1)
  x + 1
  )");
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
  f ::= (b: bool) => 4
  f := 3
  )");
  auto const *id = mod.Append<ast::Identifier>("f");
  auto const *qt = mod.context().qual_type(id);
  ASSERT_THAT(qt, Pointee(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-callable-in-overload-set"),
                           Pair("type-error", "shadowing-declaration")));
}

TEST(Identifier, CyclicDependency) {
  test::TestModule mod;
  mod.AppendCode(R"(
  x ::= y + 1
  y ::= z + 1
  z ::= x + 1
  )");
  ASSERT_THAT(mod.context().qual_type(mod.Append<ast::Identifier>("x")),
              Pointee(type::QualType::Error()));
  ASSERT_THAT(mod.context().qual_type(mod.Append<ast::Identifier>("y")),
              Pointee(type::QualType::Error()));
  ASSERT_THAT(mod.context().qual_type(mod.Append<ast::Identifier>("z")),
              Pointee(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "cyclic-dependency")));
}

TEST(Identifier, InaccessibleDeclaration) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n := 0
  f ::= () => n
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncaptured-identifier")));
}

}  // namespace
}  // namespace compiler
