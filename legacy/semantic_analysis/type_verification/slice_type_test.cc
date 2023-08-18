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

TEST(SliceType, Correct) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([/]i64)"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([/][/]i64)"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([/][/][/]i64)"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
}

TEST(SliceType, NonConstantType) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
  T := i64
  [/]T
  )"),
              AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
}

TEST(SliceType, NonTypeElement) {
  test::Repl repl;

  EXPECT_THAT(
      repl.type_check(R"([/]2)"),
      AllOf(HasQualTypes(Error(Constant(Type))),
            HasDiagnostics(Pair("type-error", "slice-data-type-not-a-type"))));
}

}  // namespace
}  // namespace semantic_analysis
