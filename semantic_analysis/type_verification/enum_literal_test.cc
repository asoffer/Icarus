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


TEST(EnumLiteral, Empty) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(enum {})"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}

TEST(EnumLiteral, SimpleSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(enum { A \\ B \\ C })"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}

TEST(EnumLiteral, SimpleSuccessWithValues) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(enum { A \\ B ::= 3 \\ C })"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}


TEST(EnumLiteral, InvalidValues) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(enum { A \\ B ::= "x" \\ C ::= 3.1 })"),
      AllOf(HasQualTypes(Constant(Type)),
            HasDiagnostics(Pair("type-error", "non-integral-enumerator"),
                           Pair("type-error", "non-integral-enumerator"))));
}

TEST(EnumLiteral, NonConstantValue) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
        n := 3 as u64
        enum { A \\ B \\ C ::= n }
      )"),
      AllOf(HasQualTypes(Constant(Type)),
            HasDiagnostics(Pair("type-error", "non-constant-enumerator"))));
}

}  // namespace
}  // namespace semantic_analysis
