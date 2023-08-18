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

TEST(Access, PresentEnumElementWithoutValue) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  e ::= enum { A \\ B ::= 1 }
  e.A
  )"),
              AllOf(HasQualTypes(Constant(
                        core::Type(GlobalTypeSystem.index<EnumType>(), 0))),
                    HasDiagnostics()));
}

TEST(Access, PresentEnumElementWithValue) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  e ::= enum { A \\ B ::= 1 }
  e.B
  )"),
              AllOf(HasQualTypes(Constant(
                        core::Type(GlobalTypeSystem.index<EnumType>(), 0))),
                    HasDiagnostics()));
}

TEST(Access, MissingEnumElement) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
  e ::= enum { A \\ B ::= 1 }
  e.C
  )"),
      AllOf(HasQualTypes(Error(
                Constant(core::Type(GlobalTypeSystem.index<EnumType>(), 0)))),
            HasDiagnostics(Pair("type-error", "missing-constant-member"))));
}

}  // namespace
}  // namespace semantic_analysis
