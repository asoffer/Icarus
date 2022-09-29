#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/slice.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

TEST(Call, NoParameters) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"((() => true)())"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"('(() => true))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"((() => true)(true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(true'(() => true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"((() => true)(b = true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"((b = true)'(() => true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
}

TEST(Call, OneParameterWithoutImplicitConversions) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(((b: bool) => b)(true))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(((b: bool) => b)(b = true))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(true'((b: bool) => b))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((b = true)'((b: bool) => b))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));

  EXPECT_THAT(
      repl.type_check(R"(((b: bool) => b)())"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"('((b: bool) => b))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"((x = true)'((b: bool) => b))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(((b: bool) => b)(x = true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(((b: bool) => b)(1.0))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(((b: bool) => b)(b = 1.0))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(1.0'((b: bool) => b))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
}

TEST(Call, OneParameterWithDefaults) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(((b := true) => b)())"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"('((b := true) => b))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
}

TEST(Call, MultipleParametersWithoutImplicitConversions) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(((b: bool, f: f64) => b)(true, 1.0))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(((b: bool, f: f64) => b)(b = true, f = 1.0))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(((b: bool, f: f64) => b)(true, f = 1.0))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(((b: bool, f: f64) => b)(f = 1.0, b = true))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(true'((b: bool, f: f64) => b)(1.0))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(true'((b: bool, f: f64) => b)(f = 1.0))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"((b = true)'((b: bool, f: f64) => b)(f = 1.0))"),
      AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"((f = 1.0)'((b: bool, f: f64) => b)(b = true))"),
      AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((true, 1.0)'((b: bool, f: f64) => b))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((true, f = 1.0)'((b: bool, f: f64) => b))"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"((b = true, f = 1.0)'((b: bool, f: f64) => b))"),
      AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(
      repl.type_check(R"((f = 1.0, b = true)'((b: bool, f: f64) => b))"),
      AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));

  EXPECT_THAT(
      repl.type_check(R"(((b: bool, f: f64) => b)(1.0, true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(((b: bool, f: f64) => b)(1.0, b = true))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"((f = true, b = 1.0)'((b: bool, f: f64) => b))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));

  EXPECT_THAT(
      repl.type_check(R"(((b: bool, f: f64) => b)(x = true, f = 1.0))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
  EXPECT_THAT(
      repl.type_check(R"(true'((b: bool, f: f64) => b)(x = 1.0))"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
}

}  // namespace
}  // namespace semantic_analysis
