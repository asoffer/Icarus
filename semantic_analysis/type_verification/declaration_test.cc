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
  // TODO: Type must be default initializable.
  // TODO: Parameters
}

TEST(Declaration, Inferred) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(x := true)"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(x ::= true)"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));

  // Cannot have a non-constant value initializing a constant.
  EXPECT_THAT(
      repl.type_check(R"(
  b := true
  x ::= b
  )"),
      AllOf(HasQualTypes(Error(Constant(Bool))),
            HasDiagnostics(Pair("value-category-error",
                                "initializing-constant-with-nonconstant"))));

  // TODO: Uninferrable types.
  // TODO: Parameters.
  // EXPECT_THAT(repl.type_check(R"(x := [])"),
  //             AllOf(HasQualTypes(QualifiedType(I(32))), HasDiagnostics()));
  // EXPECT_THAT(repl.type_check(R"(x ::= [])"),
  //             AllOf(HasQualTypes(Constant(U(64))), HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
