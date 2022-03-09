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
  auto &mod      = infra.add_module(R"(
  n: i64
  n
  )");
  auto const *id = mod.get<ast::Identifier>();
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(mod.context().decls(id), SizeIs(1));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Identifier, Undeclared) {
  test::CompilerInfrastructure infra;
  auto &mod      = infra.add_module(R"(n)");
  auto const *id = mod.get<ast::Identifier>();
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "undeclared-identifier")));
}

TEST(Identifier, UndeclaredDoesNotRepeat) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  x := f(1)
  x + 1
  )");
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "undeclared-identifier")));
}

TEST(Identifier, OverloadSetSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod      = infra.add_module(R"(
  f := () => 3
  f ::= (b: bool) => 4
  f
  )");
  auto const *id = mod.get<ast::Identifier>();
  auto qts       = mod.context().qual_types(id);
  EXPECT_TRUE(qts[0].type().is<type::OverloadSet>());
  EXPECT_THAT(mod.context().decls(id), SizeIs(2));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Identifier, NonCallableOverloads) {
  test::CompilerInfrastructure infra;
  auto &mod      = infra.add_module(R"(
  f ::= (b: bool) => 4
  f := 3
  f
  )");
  auto const *id = mod.get<ast::Identifier>();
  auto qts       = mod.context().qual_types(id);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "shadowing-declaration")));
}

TEST(Identifier, CyclicDependency) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  x ::= y + 1
  y ::= z + 1
  z ::= x + 1

  y
  )");
  ASSERT_THAT(mod.context().qual_types(mod.get<ast::Identifier>()),
              UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "cyclic-dependency")));
}

TEST(Identifier, InaccessibleDeclaration) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n := 0
  f ::= () => n
  )");
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "uncaptured-identifier")));
}

}  // namespace
}  // namespace compiler
