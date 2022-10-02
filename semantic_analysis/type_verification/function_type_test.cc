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

TEST(FunctionType, Correct) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(() -> ())"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((i64, bool) -> (f32, f64))"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((n: i64, b: bool) -> (f32, f64))"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(
  some_type: type
  (n: i64, some_type) -> (f32, f64)
  )"),
              AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
}

TEST(FunctionType, NonTypeParameter) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"((true, b: bool) -> (f32, 4))"),
      AllOf(HasQualTypes(Error(Constant(Type))),
            HasDiagnostics(Pair("type-error", "non-type-function-input"),
                           Pair("type-error", "non-type-function-output"))));
}

}  // namespace
}  // namespace compiler
