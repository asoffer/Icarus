#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::_;
using ::testing::AllOf;
using ::testing::Pair;

TEST(WhileStmt, Invalid) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(while (5) {})"),
      AllOf(HasQualTypes(),
            HasDiagnostics(Pair("type-error", "non-boolean-condition"))));
}

TEST(WhileStmt, ErrorsInBody) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
    while (true) {
     -true
    })"),
              AllOf(HasQualTypes(), HasDiagnostics(_)));
}

}  // namespace
}  // namespace semantic_analysis
