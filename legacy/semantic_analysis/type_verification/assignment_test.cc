#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::testing::Pair;

TEST(Assignment, SimpleSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n: i64
  n = n
  )"),
              HasDiagnostics());
}

TEST(Assignment, NonReference) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  f ::= () => 3 as i64
  f() = 4 as i64
  )"),
              HasDiagnostics(
                  Pair("value-category-error", "assigning-to-non-reference")));
}

TEST(Assignment, Constant) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n ::= 3 as i64
  n = 0
  )"),
              HasDiagnostics(
                  Pair("value-category-error", "assigning-to-non-reference")));
}

TEST(Assignment, WithCast) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n: [*]i64
  m: *i64
  (m, n) = (n, n)
  )"),
              HasDiagnostics());
}

TEST(Assignment, TypeMismatch) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n := 3
  n = "hello"
  )"),
              HasDiagnostics(Pair("type-error", "assignment-type-mismatch")));
}

// TODO: Significantly more tests covering expression expansion.

}  // namespace
}  // namespace semantic_analysis
