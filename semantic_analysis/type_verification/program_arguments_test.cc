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

TEST(ProgramArguments, Access) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(arguments)"),
      AllOf(HasQualTypes(QualifiedType(SliceType(repl.type_system(), Char))),
            HasDiagnostics()));
}

TEST(ProgramArguments, DISABLED_AccessibleInLocalScope) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
      if (true) {
        arguments[0]
      }
      )"),
      AllOf(HasQualTypes(QualifiedType(SliceType(repl.type_system(), Char))),
            HasDiagnostics()));
}

TEST(ProgramArguments, InaccessibleBehindScopeBoundary) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
      () => arguments
      )"),
              HasDiagnostics(Pair("access-error", "program-arguments-access")));
}

TEST(ProgramArguments, DISABLED_CannotUseAsIdentifier) {
  // Behind a function `arguments` should be inaccessible, but the keyword
  // should still be treated as reserved.
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  () -> () {
    arguments := 3
  }
  )"),
              HasDiagnostics(Pair("parse-error", "declaring-non-identifier")));
}

}  // namespace
}  // namespace semantic_analysis

