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

TEST(ArrayLiteral, EmptyArray) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"([])"),
              AllOf(HasQualTypes(Constant(EmptyArray)), HasDiagnostics()));
}

TEST(ArrayLiteral, OneElement) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"([0])"),
      AllOf(HasQualTypes(Constant(ArrayType(GlobalTypeSystem, 1, Integer))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"([true])"),
      AllOf(HasQualTypes(Constant(ArrayType(GlobalTypeSystem, 1, Bool))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"([[true]])"),
      AllOf(HasQualTypes(Constant(ArrayType(
                GlobalTypeSystem, 1, ArrayType(GlobalTypeSystem, 1, Bool)))),
            HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"([[[]]])"),
              AllOf(HasQualTypes(Constant(
                        ArrayType(GlobalTypeSystem, 1,
                                  ArrayType(GlobalTypeSystem, 1, EmptyArray)))),
                    HasDiagnostics()));
}

TEST(ArrayLiteral, MultipleMatchingElements) {
  test::Repl repl;

  EXPECT_THAT(
      repl.type_check(R"([0, 0])"),
      AllOf(HasQualTypes(Constant(ArrayType(GlobalTypeSystem, 2, Integer))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"(
      a: bool
      [a, false]
      )"),
      AllOf(HasQualTypes(QualifiedType(ArrayType(GlobalTypeSystem, 2, Bool))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"([true, false])"),
      AllOf(HasQualTypes(Constant(ArrayType(GlobalTypeSystem, 2, Bool))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"([[true], [false]])"),
      AllOf(HasQualTypes(Constant(ArrayType(
                GlobalTypeSystem, 2, ArrayType(GlobalTypeSystem, 1, Bool)))),
            HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"(
      b: bool
      [[b], [false]]
      )"),
      AllOf(HasQualTypes(QualifiedType(ArrayType(
                GlobalTypeSystem, 2, ArrayType(GlobalTypeSystem, 1, Bool)))),
            HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"([[[]], [[]], [[]]])"),
              AllOf(HasQualTypes(Constant(
                        ArrayType(GlobalTypeSystem, 3,
                                  ArrayType(GlobalTypeSystem, 1, EmptyArray)))),
                    HasDiagnostics()));
}

TEST(ArrayLiteral, ElementTypeMismatch) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"([true, 0.0])"),
              AllOf(HasQualTypes(Constant(Error())),
                    HasDiagnostics(Pair("type-error",
                                        "inconsistent-array-element-type"))));
  EXPECT_THAT(repl.type_check(R"(
  b: bool
  [b, 0.0]
  )"),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair("type-error",
                                        "inconsistent-array-element-type"))));

  EXPECT_THAT(repl.type_check(R"(
  n: i64
  [n, 0.0]
  )"),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair("type-error",
                                        "inconsistent-array-element-type"))));
}

TEST(ArrayLiteral, Recovery) {
  test::Repl repl;

  // There are sufficiently many boolean values here that it is reasonable to
  // guess that the one non-boolean value is the mistake.
  //
  // Guessing intended types is a heuristic that we intend to tweak over time.
  EXPECT_THAT(
      repl.type_check(R"([true, true, false, 0.0, true])"),
      AllOf(HasQualTypes(Error(Constant(ArrayType(GlobalTypeSystem, 5, Bool)))),
            HasDiagnostics(
                Pair("type-error", "inconsistent-array-element-type"))));
}

}  // namespace
}  // namespace semantic_analysis
