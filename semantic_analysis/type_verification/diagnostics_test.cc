#include "semantic_analysis/type_verification/diagnostics.h"

#include "absl/strings/str_cat.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

struct TestCase {
  std::string context;
  std::string expr;
  std::string expected;
};

using TypeForDiagnosticTest = testing::TestWithParam<TestCase>;
TEST_P(TypeForDiagnosticTest, Test) {
  auto const &[context, expr, expected] = GetParam();
  test::Repl repl;
  repl.type_check(absl::StrCat(context, "\n", expr));

  EXPECT_EQ(TypeForDiagnostic(repl.last_expression(), repl.context(),
                              repl.type_system()),
            expected);
}
INSTANTIATE_TEST_SUITE_P(All, TypeForDiagnosticTest,
                         testing::ValuesIn(std::vector<TestCase> {
                             {
                                 .context  = "n: i64",
                                 .expr     = "n",
                                 .expected = "i64",
                             },
// TODO Reimplement
#if 0
                             {
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "n",
                                 .expected = "Int",
                             },
                             {
                                 .context  = "b := true",
                                 .expr     = "b",
                                 .expected = "bool",
                             },
                             {
                                 .context  = R"(
                                 S ::= struct {}
                                 )",
                                 .expr     = "S.{}",
                                 .expected = "S",
                             },
                             {
                                 .context  = R"(
                                 S ::= struct {}
                                 Alias ::= S
                                 )",
                                 .expr     = "Alias.{}",
                                 .expected = "Alias",
                             },
                             {
                                 .context  = R"(
                                 E ::= enum { A }
                                 e := E.A
                                 )",
                                 .expr     = "e",
                                 .expected = "E",
                             },
                             {
                                 .context  = R"(
                                 F ::= flags { A }
                                 f := F.A
                                 )",
                                 .expr     = "f",
                                 .expected = "F",
                             },
                             {
                                 .expr     = "[0 as i64]",
                                 .expected = "[1; i64]",
                             },
                             {
                                 .expr     = "[1, 2, 3]",
                                 .expected = "[3; integer]",
                             },
                             {
                                 .context  = R"(
                                 I ::= i64
                                 n: I
                                 )",
                                 .expr     = "[n, n, n]",
                                 .expected = "[3; I]",
                             },
                             {
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "n + n",
                                 .expected = "Int",
                             },
                             {
                                 .context  = R"(Int ::= i64 \\ n: Int)",
                                 .expr     = "-n",
                                 .expected = "Int",
                             },
                             {
                                 .context  = R"(
                                 Int ::= i64
                                 f ::= () -> Int { return 0 }
                                 )",
                                 .expr     = "f()",
                                 .expected = "Int",
                             },
#endif
                         }));

}  // namespace
}  // namespace semantic_analysis
