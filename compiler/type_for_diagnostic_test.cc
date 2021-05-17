#include "compiler/type_for_diagnostic.h"

#include "compiler/library_module.h"
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
                                 .expr     = "[1, 2, 3]",
                                 .expected = "[3; i64]",
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
  auto id1 = ir::ModuleId::New();
  auto id2 = ir::ModuleId::New();
  LibraryModule imported_mod1;
  LibraryModule imported_mod2;
  imported_mod1.set_diagnostic_consumer<diagnostic::TrackingConsumer>();
  imported_mod2.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported1")))
      .WillByDefault([id1](std::string_view) { return id1; });
  ON_CALL(mod.importer, Import(Eq("imported2")))
      .WillByDefault([id2](std::string_view) { return id2; });
  ON_CALL(mod.importer, get(id1))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod1; });
  ON_CALL(mod.importer, get(id2))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod2; });

  frontend::SourceBuffer buffer1(R"(
  #{export} S ::= struct {}
  )");
  imported_mod1.AppendNodes(frontend::Parse(buffer1, mod.consumer),
                            mod.consumer, mod.importer);

  frontend::SourceBuffer buffer2(R"(
  #{export} S ::= struct {}
  #{export} P ::= struct (T :: type) {}
  )");
  imported_mod2.AppendNodes(frontend::Parse(buffer2, mod.consumer),
                            mod.consumer, mod.importer);

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
