#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;
using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

TEST(SliceType, Correct) {
  {
    test::Snippet snippet(R"([/]i64)");
    ASSERT_THAT(snippet, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(snippet,
                EvaluatesTo<core::Type>(SliceType(GlobalTypeSystem, I(64))));
  }
  {
    test::Snippet snippet(R"([/][/]i64)");
    ASSERT_THAT(snippet, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(snippet,
                EvaluatesTo<core::Type>(SliceType(
                    GlobalTypeSystem, SliceType(GlobalTypeSystem, I(64)))));
  }
  {
    test::Snippet snippet(R"([/][/][/]i64)");
    ASSERT_THAT(snippet, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(
        snippet,
        EvaluatesTo<core::Type>(SliceType(
            GlobalTypeSystem,
            SliceType(GlobalTypeSystem, SliceType(GlobalTypeSystem, I(64))))));
  }
}

// TEST(SliceType, NonConstantType) {
//   test::Repl repl;
// 
//   EXPECT_THAT(repl.type_check(R"(
//   T := i64
//   [/]T
//   )"),
//               AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
// }
// 
// TEST(SliceType, NonTypeElement) {
//   test::Repl repl;
// 
//   EXPECT_THAT(
//       repl.type_check(R"([/]2)"),
//       AllOf(HasQualTypes(Error(Constant(Type))),
//             HasDiagnostics(Pair("type-error", "slice-data-type-not-a-type"))));
// }

}  // namespace
}  // namespace semantic_analysis
