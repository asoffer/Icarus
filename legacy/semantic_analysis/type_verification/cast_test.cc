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

TEST(Cast, Integers) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(3 as i8)"),
              AllOf(HasQualTypes(Constant(I(8))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(3 as u8)"),
              AllOf(HasQualTypes(Constant(U(8))), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(3 as i64)"),
              AllOf(HasQualTypes(Constant(I(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(3 as u64)"),
              AllOf(HasQualTypes(Constant(U(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(3 as integer)"),
              AllOf(HasQualTypes(Constant(Integer)), HasDiagnostics()));
}

TEST(Cast, NonType) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(3 as true)"),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair("type-error", "non-type-in-cast"))));
}

TEST(Cast, NonConstantType) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
  T := i8  // Non-constant type
  3 as T
  )"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "non-constant-type-in-cast"))));
}

}  // namespace
}  // namespace semantic_analysis
