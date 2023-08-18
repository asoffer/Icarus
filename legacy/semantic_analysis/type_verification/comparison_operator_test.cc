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

TEST(ComparisonOperator, DifferentTypes) {
  {
    // `i32` and `i64` are comparable because the set of representable values of
    // one contains the other.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x := 1 as i64
    y := 2 as i32

    x < y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // `u32` and `i64` are comparable because `u32` can be sign-extended to
    // `i64` and then compared.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x := 1 as u32
    y := 2 as i64

    x < y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // `u32` and `i32` are comparable. Morally we can lift them up into a larger
    // representation space and give a meaningful result. In practice we don't
    // need to actually do the lift. It can be done with a few instructions with
    // all data in registers.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x := 1 as u32
    y := 2 as i64

    x < y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // `u32` and `bool` are not comparable.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x := 1 as u32
    y := true

    x < y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
}

TEST(ComparisonOperator, Pointers) {
  {
    // Pointers to the same type are comparable for equality.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: *i64
    x == x != x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Pointers to different types are not comparable for equality.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: *i64
    y: *i32

    x == y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
  {
    // Pointers to the same type are not ordered
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: *i64
    x < x
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
  {
    // Pointers and buffer pointers to the same type are comparable for
    // equality.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: [*]i64
    y: *i64
    x == y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Pointers and buffer pointers to the same type are comparable for
    // non-equality.
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: *i64
    y: [*]i64
    x != y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Pointers and buffer pointers to the same type are comparable for
    // equality.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: [*]i64
    y: *i32
    x == y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
  {
    // Pointers are not ordered with buffer pointers.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: [*]i64
    y: *i64
    x < y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
  {
    // Comparing buffer pointers for equality
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: [*]i64
    x == x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Comparing buffer pointers for non-equality
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: *i64
    y: [*]i64
    x != y
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Comparing buffer pointers for inequality
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    x: [*]i64
    x < x <= x == x != x >= x > x
    )"),
                AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  }
  {
    // Pointers and buffer pointers to the distinct types are not comparable.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: [*]i64
    y: [*]i32
    x == y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
  {
    // Pointers and buffer pointers to the distinct types are not comparable.
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    x: [*]i64
    y: [*]i32
    x >= y
    )"),
        AllOf(HasQualTypes(Error(Bool)),
              HasDiagnostics(Pair("type-error", "comparing-incomparables"))));
  }
}

TEST(ComparisonOperator, MultipleErrorsInChain) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
  x: i32
  y: bool

  x >= y > x == x < y == y
  )"),
      AllOf(HasQualTypes(Error(Bool)),
            HasDiagnostics(Pair("type-error", "comparing-incomparables"),
                           Pair("type-error", "comparing-incomparables"),
                           Pair("type-error", "comparing-incomparables"))));
}

TEST(ComparisonOperator, Float) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  x: f32
  y: f64

  x >= y > x == x < y == y
  )"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
}

// TODO: Tests operator overloading, error propogation, and overloads returning the wrong type.

}  // namespace
}  // namespace semantic_analysis
