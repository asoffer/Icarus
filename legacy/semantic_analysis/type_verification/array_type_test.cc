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

TEST(ArrayType, Correct) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([3; i64])"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([2; [3; i64]])"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"([1, 2, 3; i64])"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
  n :: i8
  [n; i64]
  )"),
                AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  }
}

TEST(ArrayType, NonConstantType) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
  T := i64
  [3; T]
  )"),
              AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
}

TEST(ArrayType, NonTypeElement) {
  test::Repl repl;

  EXPECT_THAT(
      repl.type_check(R"([3; 2])"),
      AllOf(HasQualTypes(Error(Constant(Type))),
            HasDiagnostics(Pair("type-error", "array-data-type-not-a-type"))));
}

TEST(ArrayType, NonConstantLength) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
      n := 3
      [3, n, 2; i64]
      )"),
              AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
}

TEST(ArrayType, NonIntegerLength) {
  {
    test::Repl repl;

    EXPECT_THAT(
        repl.type_check(R"([3.0; i64])"),
        AllOf(HasQualTypes(Error(Constant(Type))),
              HasDiagnostics(Pair("type-error", "non-integral-array-length"))));
  }
  {
    test::Repl repl;

    EXPECT_THAT(
        repl.type_check(R"(
      x := 3.0
      [x; i64])"),
        AllOf(HasQualTypes(Error(QualifiedType(Type))),
              HasDiagnostics(Pair("type-error", "non-integral-array-length"))));
  }
}

}  // namespace
}  // namespace semantic_analysis
