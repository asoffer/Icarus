#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::_;
using ::testing::AllOf;
using ::testing::Pair;

TEST(IfStmt, Trivial) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(if (true) {})"),
              AllOf(HasQualTypes(), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(if (true) {} else {})"),
              AllOf(HasQualTypes(), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(#{const} if (true) {})"),
              AllOf(HasQualTypes(), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(#{const} if (true) {} else {})"),
              AllOf(HasQualTypes(), HasDiagnostics()));
}

TEST(IfStmt, Invalid) {
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(if (5) {})"),
        AllOf(HasQualTypes(),
              HasDiagnostics(Pair("type-error", "non-boolean-condition"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(if (5) {} else {})"),
        AllOf(HasQualTypes(),
              HasDiagnostics(Pair("type-error", "non-boolean-condition"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(#{const} if (5) {})"),
        AllOf(HasQualTypes(),
              HasDiagnostics(Pair("type-error", "non-boolean-condition"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(#{const} if (5) {} else {})"),
        AllOf(HasQualTypes(),
              HasDiagnostics(Pair("type-error", "non-boolean-condition"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
        b: bool
        #{const} if (b) {} else {}
        )"),
                AllOf(HasQualTypes(),
                      HasDiagnostics(
                          Pair("type-error", "non-constant-condition-error"))));
  }
}

TEST(IfStmt, ErrorsInBody) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    if (true) {
     -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics(_)));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    if (true) {
      -true
    } else {
      -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics(_, _)));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (true) {
      -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics(_)));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (false) {
      -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (true) {
      -true
    } else {
    })"),
                AllOf(HasQualTypes(), HasDiagnostics(_)));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (false) {
      -true
    } else {
    })"),
                AllOf(HasQualTypes(), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (true) {
    } else {
      -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics()));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    #{const} if (false) {
    } else {
      -true
    })"),
                AllOf(HasQualTypes(), HasDiagnostics(_)));
  }
}

TEST(IfStmt, UnreachableConst) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(() -> bool {
      #{const} if (true) {
       return true
      }
      1234
      return true
    })"),
                HasDiagnostics(Pair("type-error", "unreachable-statement")));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(() -> bool {
      #{const} if (false) {
      } else {
       return true
      }
      1234
      return true
    })"),
                HasDiagnostics(Pair("type-error", "unreachable-statement")));
  }
}

TEST(IfStmt, Unreachable) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(() -> bool {
      if (false) {
       return true
      } else {
       return true
      }
      1234
      return true
    })"),
                HasDiagnostics(Pair("type-error", "unreachable-statement")));
  }
}

}  // namespace
}  // namespace semantic_analysis
