#include "compiler/type_for_diagnostic.h"

#include "compiler/module.h"
#include "diagnostic/consumer/tracking.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {
using ::testing::Eq;

struct TestCase {
  std::string context;
  std::string expr;
  std::string expected;
};

using TypeForDiagnosticTest = testing::TestWithParam<TestCase>;
TEST_P(TypeForDiagnosticTest, Test) {
  auto const &[context, expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(context);
  auto const *e = mod.Append<ast::Expression>(expr);
  EXPECT_EQ(TypeForDiagnostic(e, mod.context()), expected);
}
INSTANTIATE_TEST_SUITE_P(All, TypeForDiagnosticTest,
                         testing::ValuesIn({
                             TestCase{
                                 .context  = "n: i64",
                                 .expr     = "n",
                                 .expected = "i64",
                             },
                             TestCase{
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "n",
                                 .expected = "Int",
                             },
                             TestCase{
                                 .context  = "b := true",
                                 .expr     = "b",
                                 .expected = "bool",
                             },
                             TestCase{
                                 .context  = R"(
                                 S ::= struct {}
                                 )",
                                 .expr     = "S.{}",
                                 .expected = "S",
                             },
                             TestCase{
                                 .context  = R"(
                                 S ::= struct {}
                                 Alias ::= S
                                 )",
                                 .expr     = "Alias.{}",
                                 .expected = "Alias",
                             },
                             TestCase{
                                 .context  = R"(
                                 E ::= enum { A }
                                 e := E.A
                                 )",
                                 .expr     = "e",
                                 .expected = "E",
                             },
                             TestCase{
                                 .context  = R"(
                                 F ::= flags { A }
                                 f := F.A
                                 )",
                                 .expr     = "f",
                                 .expected = "F",
                             },
                             TestCase{
                                 .expr     = "[0 as i64]",
                                 .expected = "[1; i64]",
                             },
                             TestCase{
                                 .expr     = "[1, 2, 3]",
                                 .expected = "[3; integer]",
                             },
                             TestCase{
                                 .context  = R"(
                                 I ::= i64
                                 n: I
                                 )",
                                 .expr     = "[n, n, n]",
                                 .expected = "[3; I]",
                             },
                             TestCase{
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "n + n",
                                 .expected = "Int",
                             },
                             TestCase{
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "-n",
                                 .expected = "Int",
                             },
                             TestCase{
                                 .context  = R"(
                                 Int ::= i64
                                 S ::= struct (T :: type) {}
                                 n: Int
                                 s: S(n:?)
                                 )",
                                 .expr     = "s",
                                 .expected = "S(Int)",
                             },
                             TestCase{
                                 .context  = R"(
                                 Int ::= i64
                                 S ::= struct (T :: type) {}
                                 n: Int
                                 s: n:?'S
                                 )",
                                 .expr     = "s",
                                 .expected = "S(Int)",
                             },
                             TestCase{
                                 .context  = R"(
                                 Int ::= i64
                                 S ::= struct (T :: type) {}
                                 n: Int
                                 s: S(T = n:?)
                                 )",
                                 .expr     = "s",
                                 .expected = "S(T = Int)",
                             },
                             TestCase{
                                 .context  = R"(
                                 Int ::= i64
                                 f ::= () -> Int { return 0 }
                                 )",
                                 .expr     = "f()",
                                 .expected = "Int",
                             },
                         }));

TEST(CrossModule, TypeForDiagnostic) {
  test::TestModule mod;

  CompiledModule imported_mod1;
  mod.CompileImportedLibrary(imported_mod1, "imported1", R"(
  #{export} S ::= struct {}
  )");

  CompiledModule imported_mod2;
  mod.CompileImportedLibrary(imported_mod2, "imported2", R"(
  #{export} S ::= struct {}
  #{export} P ::= struct (T :: type) {}
  )");

  mod.AppendCode(R"(
  --  ::= import "imported1"
  mod ::= import "imported2"
  )");

  auto const *module_access   = mod.Append<ast::Expression>(R"(mod.S.{})");
  EXPECT_EQ(TypeForDiagnostic(module_access, mod.context()), "mod.S");

  auto const *embedded_access = mod.Append<ast::Expression>(R"(S.{})");
  EXPECT_EQ(TypeForDiagnostic(embedded_access, mod.context()), "S");

  // TODO: Deal with ADL.
}

}  // namespace
}  // namespace compiler
