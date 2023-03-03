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

TEST(Call, OneParameterWithImplicitConversions) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(((n: i64) => n)(3))"),
              AllOf(HasQualTypes(QualifiedType(I(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(((n: i64) => n)(n = 3))"),
              AllOf(HasQualTypes(QualifiedType(I(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(3'((n: i64) => n))"),
              AllOf(HasQualTypes(QualifiedType(I(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"((n = 3)'((n: i64) => n))"),
              AllOf(HasQualTypes(QualifiedType(I(64))), HasDiagnostics()));
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

TEST(Call, BuiltinForeign) {
  test::Repl repl;

  core::Parameters<core::Type> parameters;
  parameters.append("", I(32));

  core::FunctionType fn_type(
      repl.type_system(),
      core::ParameterType(repl.type_system(), std::move(parameters)), {I(32)});

  EXPECT_THAT(repl.type_check(R"(
  builtin.foreign("func", i32 -> i32)
  )"),
              AllOf(HasQualTypes(Constant(fn_type)), HasDiagnostics()));
}

TEST(Call, BuiltinForeignPointer) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
  builtin.foreign("errno", *i32)
  )"),
              AllOf(HasQualTypes(
                        QualifiedType(core::PointerType(repl.type_system(), I(32)))),
                    HasDiagnostics()));
}

TEST(Call, BuiltinForeignBufferPointer) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
  builtin.foreign("something", [*]bool)
  )"),
              AllOf(HasQualTypes(QualifiedType(
                        BufferPointerType(repl.type_system(), Bool))),
                    HasDiagnostics()));
}

TEST(Call, OverloadSets) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(
  f ::= () => true
  f ::= (n: i32) => n
  f()
  )"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(
  x: i32
  g ::= () => true
  g ::= (n: i32) => n
  g(x)
  )"),
              AllOf(HasQualTypes(QualifiedType(I(32))), HasDiagnostics()));

  EXPECT_THAT(
      repl.type_check(R"(
  h ::= (b: bool) => b
  h ::= (n: i32) => n
  h()
  )"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "uncallable-with-arguments"))));
}

}  // namespace
}  // namespace semantic_analysis
