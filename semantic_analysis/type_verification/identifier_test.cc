#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

TEST(Identifier, Builtin) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(builtin)"),
              AllOf(HasQualTypes(Constant(Module)), HasDiagnostics()));
}

TEST(Identifier, Success) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n: i64
  n
  )"),
              AllOf(HasQualTypes(Reference(I(64))), HasDiagnostics()));
}

TEST(Identifier, Undeclared) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(n)"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "undeclared-identifier"))));
}

#if 0
TEST(Identifier, UndeclaredDoesNotRepeat) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  x := f(1)
  x + 1
  )");
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "undeclared-identifier")));
}

TEST(Identifier, DISABLED_OverloadSetSuccess) {
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
  x := y + 1
  y := z + 1
  z := x + 1

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

#endif

}  // namespace
}  // namespace semantic_analysis
