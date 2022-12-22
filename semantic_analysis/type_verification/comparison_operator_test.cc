#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::UnorderedElementsAre;

TEST(ComparisonOperator, ConstantSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  x ::= 1 as i64
  y ::= 2 as i64

  x < y <= 3 == x != y >= 3 > x
  )"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
}

TEST(ComparisonOperator, NonConstantSuccess) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x := 1 as i64
    y ::= 2 as i64

    x < y <= 3 == x != y >= 3 > x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x ::= 1 as i64
    y := 2 as i64

    x < y <= 3 == x != y >= 3 > x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x := 1 as i64
    y := 2 as i64

    x < y <= 3 == x != y >= 3 > x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
}

// TODO: Tests for floating-point, pointers, buffer pointers, operator overloading,
// error propogation, and overloads returning the wrong type.

}  // namespace
}  // namespace semantic_analysis
