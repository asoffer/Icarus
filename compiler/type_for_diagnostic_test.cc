#include "compiler/type_for_diagnostic.h"

#include "compiler/library_module.h"
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
                         }));

TEST(CrossModule, TypeForDiagnostic) {
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {}
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer), mod.consumer,
                           mod.importer);

  mod.AppendCode(R"(
  mod ::= import "imported"
  -- ::= mod
  )");
  auto const *module_access   = mod.Append<ast::Expression>(R"(mod.S.{})");
  auto const *embedded_access = mod.Append<ast::Expression>(R"(S.{})");
  EXPECT_EQ(TypeForDiagnostic(module_access, mod.context()), "mod.S");
  EXPECT_EQ(TypeForDiagnostic(embedded_access, mod.context()), "S");
}

}  // namespace
}  // namespace compiler
