#include "absl/strings/str_format.h"
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

using BinaryOperatorSameTypeTest =
    testing::TestWithParam<std::tuple<core::Type, char const *>>;

std::string_view StringOf(core::Type t) {
  if (t == Bool) { return "bool"; }
  if (t == I(8)) { return "i8"; }
  if (t == I(16)) { return "i16"; }
  if (t == I(32)) { return "i32"; }
  if (t == I(64)) { return "i64"; }
  if (t == U(8)) { return "u8"; }
  if (t == U(16)) { return "u16"; }
  if (t == U(32)) { return "u32"; }
  if (t == U(64)) { return "u64"; }
  if (t == F32) { return "f32"; }
  if (t == F64) { return "f64"; }
  UNREACHABLE();
}

TEST_P(BinaryOperatorSameTypeTest, ConstantSuccess) {
  test::Repl repl;
  auto [type, op] = GetParam();
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x ::= 1 as %s
  x %s x
  )",
                                              StringOf(type), op)),
              AllOf(HasQualTypes(Constant(type)), HasDiagnostics()));
}

TEST_P(BinaryOperatorSameTypeTest, MixedConstnessSuccess) {
  test::Repl repl;
  auto [type, op] = GetParam();
  EXPECT_THAT(
      repl.type_check(absl::StrFormat(R"(
  x ::= 1 as %s
  y := 2 as %s
  x %s y
  )",
                                      StringOf(type), StringOf(type), op)),
      AllOf(HasQualTypes(QualifiedType(type)), HasDiagnostics()));
}

TEST_P(BinaryOperatorSameTypeTest, NonConstSuccess) {
  test::Repl repl;
  auto [type, op] = GetParam();
  EXPECT_THAT(
      repl.type_check(absl::StrFormat(R"(
  x := 1 as %s
  y := 2 as %s
  x %s y
  )",
                                      StringOf(type), StringOf(type), op)),
      AllOf(HasQualTypes(QualifiedType(type)), HasDiagnostics()));
}

TEST_P(BinaryOperatorSameTypeTest, NonConstSuccessWithInteger) {
  test::Repl repl;
  auto [type, op] = GetParam();
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x := 1 as %s
  x %s 3
  )",
                                              StringOf(type), op)),
              AllOf(HasQualTypes(QualifiedType(type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  y := 1 as %s
  3 %s y
  )",
                                              StringOf(type), op)),
              AllOf(HasQualTypes(QualifiedType(type)), HasDiagnostics()));
}

TEST_P(BinaryOperatorSameTypeTest, ConstantSuccessWithInteger) {
  test::Repl repl;
  auto [type, op] = GetParam();
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x ::= 1 as %s
  x %s 3
  )",
                                              StringOf(type), op)),
              AllOf(HasQualTypes(Constant(type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  y ::= 1 as %s
  3 %s y
  )",
                                              StringOf(type), op)),
              AllOf(HasQualTypes(Constant(type)), HasDiagnostics()));
}

INSTANTIATE_TEST_SUITE_P(
    All, BinaryOperatorSameTypeTest,
    testing::Combine(testing::ValuesIn({I(8), I(16), I(32), I(64), U(8), U(16),
                                        U(32), U(64), F32, F64}),
                     testing::ValuesIn({"+", "-", "*", "/", "%"})));

using LogicOperatorTest = testing::TestWithParam<char const *>;

TEST_P(LogicOperatorTest, ConstantSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x ::= true
  x %s x
  )",
                                               GetParam())),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
}

TEST_P(LogicOperatorTest, MixedConstnessSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x ::= true
  y := false
  x %s y
  )",
                                               
                                              GetParam())),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
}

TEST_P(LogicOperatorTest, NonConstSuccess) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x := true
  y := false
  x %s y
  )",
                                               
                                              GetParam())),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
}

TEST_P(LogicOperatorTest, NonConstWithTypeMismatch) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x := true
  x %s 3
  )",
                                              GetParam())),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair(
                        "type-error", "logical-binary-operator-needs-bool"))));
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  y := true
  3 %s y
  )",
                                              GetParam())),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair(
                        "type-error", "logical-binary-operator-needs-bool"))));
}

TEST_P(LogicOperatorTest, ConstantWithTypeMismatch) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  x ::= 1 as f32
  x %s true
  )",
                                              GetParam())),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair(
                        "type-error", "logical-binary-operator-needs-bool"))));
  EXPECT_THAT(repl.type_check(absl::StrFormat(R"(
  y ::= 1 as i32
  true %s y
  )",
                                              GetParam())),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair(
                        "type-error", "logical-binary-operator-needs-bool"))));
}

INSTANTIATE_TEST_SUITE_P(All, LogicOperatorTest,
                         testing::ValuesIn({"and", "or", "xor"}));

}  // namespace
}  // namespace semantic_analysis
