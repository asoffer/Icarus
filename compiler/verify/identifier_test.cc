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
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n: i64
  )");
  auto const *id = mod.Append<ast::Identifier>("n");
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(mod.context().decls(id), SizeIs(1));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Identifier, Undeclared) {
  test::TestModule mod;
  auto const *id = mod.Append<ast::Identifier>("n");
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Identifier, UndeclaredDoesNotRepeat) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  x := f(1)
  x + 1
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Identifier, OverloadSetSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  f := () => 3
  f ::= (b: bool) => 4
  )");
  auto const *id = mod.Append<ast::Identifier>("f");
  auto qts       = mod.context().qual_types(id);
  EXPECT_TRUE(qts[0].type().is<type::OverloadSet>());
  EXPECT_THAT(mod.context().decls(id), SizeIs(2));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Identifier, NonCallableOverloads) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  f ::= (b: bool) => 4
  f := 3
  )");
  auto const *id = mod.Append<ast::Identifier>("f");
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
}

TEST(Identifier, CyclicDependency) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  x ::= y + 1
  y ::= z + 1
  z ::= x + 1
  )");
  ASSERT_THAT(mod.context().qual_types(mod.Append<ast::Identifier>("x")),
              UnorderedElementsAre(type::QualType::Error()));
  ASSERT_THAT(mod.context().qual_types(mod.Append<ast::Identifier>("y")),
              UnorderedElementsAre(type::QualType::Error()));
  ASSERT_THAT(mod.context().qual_types(mod.Append<ast::Identifier>("z")),
              UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "cyclic-dependency")));
}

TEST(Identifier, InaccessibleDeclaration) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n := 0
  f ::= () => n
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncaptured-identifier")));
}

}  // namespace
}  // namespace compiler
