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

std::string TypeForDiagnostic(std::string_view context,
                              std::string_view expression) {
  test::Repl repl;
  repl.type_check(absl::StrCat(context, "\n", expression));
  return TypeForDiagnostic(repl.last_expression(), repl.context(),
                           GlobalTypeSystem);
}

TEST(TypeForDiagnostic, Test) {
  EXPECT_EQ(TypeForDiagnostic(R"(n: i64)", "n"), "i64");
  EXPECT_EQ(TypeForDiagnostic(R"(Int ::= i64 \\ n: Int)", "n"), "Int");
  EXPECT_EQ(TypeForDiagnostic(R"(b := true)", "b"), "bool");
  EXPECT_EQ(TypeForDiagnostic(R"()", "[0 as i64]"), "[1; i64]");
  EXPECT_EQ(TypeForDiagnostic(R"()", "[1, 2, 3]"), "[3; integer]");
  EXPECT_EQ(TypeForDiagnostic(R"(I ::= i64 \\ n: I)", "[n, n, n]"), "[3; I]");
  // EXPECT_EQ(TypeForDiagnostic(R"(I ::= i64 \\ n: I)", "n + n"), "Int");
  EXPECT_EQ(TypeForDiagnostic(R"(Int ::= i64 \\ n: Int)", "-n"), "Int");
  EXPECT_EQ(TypeForDiagnostic(
                R"(Int ::= i64 \\ f ::= () -> Int { return 0 as i64 })", "f()"),
            "Int");
  // EXPECT_EQ(TypeForDiagnostic(R"(I ::= i64 \\ n: I)", "n + n"), "Int");
  // EXPECT_EQ(TypeForDiagnostic(R"(S ::= struct {})", "S.{}"), "S");
  // EXPECT_EQ(TypeForDiagnostic(R"(S ::= struct {} \\ Alias ::= S)",
  // "Alias.{}"), "Alias");
  // EXPECT_EQ(TypeForDiagnostic(R"(E ::= enum { A } \\ e := E.A)", "e"), "E");
  // EXPECT_EQ(TypeForDiagnostic(R"(F ::= flags { A } \\ f := F.A)", "f"), "F");
}

}  // namespace
}  // namespace semantic_analysis
