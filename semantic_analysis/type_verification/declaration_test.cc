#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;

TEST(Declaration, DefaultInitialization) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(x: bool)"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(x :: bool)"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(x: i32)"),
              AllOf(HasQualTypes(QualifiedType(I(32))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(x :: u64)"),
              AllOf(HasQualTypes(Constant(U(64))), HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
