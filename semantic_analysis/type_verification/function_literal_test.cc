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

TEST(FunctionLiteral, Correct) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(() -> () {})"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(),
                        core::ParameterType(repl.type_system(), {}), {}))),
                    HasDiagnostics()));

  core::Parameters<core::Type> parameters;
  parameters.append("n", I(64));
  parameters.append("b", Bool);
  core::ParameterType parameter_type(repl.type_system(), std::move(parameters));

  EXPECT_THAT(repl.type_check(R"((n: i64, b: bool) -> bool { return true })"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(), parameter_type, {Bool}))),
                    HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(
    (n: i64, b: bool) -> (bool, char) {
      return b, !'x'
    })"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(), parameter_type, {Bool, Char}))),
                    HasDiagnostics()));
}

TEST(FunctionLiteral, ConversionInReturn) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(() -> i32 { return 0 })"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(),
                        core::ParameterType(repl.type_system(), {}), {I(32)}))),
                    HasDiagnostics()));
}

TEST(FunctionLiteral, DISABLED_MissingReturnTypes) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(() -> bool {})"),
              AllOf(HasQualTypes(Error(Constant(core::FunctionType(
                        repl.type_system(),
                        core::ParameterType(repl.type_system(), {}), {Bool})))),
                    HasDiagnostics(Pair("type-error", "missing-return-type"))));
}

TEST(FunctionLiteral, DISABLED_IncorrectReturnType) {
  test::Repl repl;

  EXPECT_THAT(
      repl.type_check(R"(() -> bool { return !'x' })"),
      AllOf(HasQualTypes(Error(Constant(core::FunctionType(
                repl.type_system(), core::ParameterType(repl.type_system(), {}),
                {Bool})))),
            HasDiagnostics(Pair("type-error", "incorrect-return-type"))));
}

TEST(FunctionLiteralElidedReturnType, Correct) {
  test::Repl repl;

  EXPECT_THAT(repl.type_check(R"(() -> {})"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(),
                        core::ParameterType(repl.type_system(), {}), {}))),
                    HasDiagnostics()));

  core::Parameters<core::Type> parameters;
  parameters.append("n", I(64));
  parameters.append("b", Bool);
  core::ParameterType parameter_type(repl.type_system(), std::move(parameters));

  EXPECT_THAT(repl.type_check(R"((n: i64, b: bool) -> { return true })"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(), parameter_type, {Bool}))),
                    HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(
    (n: i64, b: bool) -> (bool, char) {
      return b, !'x'
    })"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        repl.type_system(), parameter_type, {Bool, Char}))),
                    HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
