#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;

TEST(FunctionType, EvaluationWithoutParameters) {
  test::Repl repl;

  EXPECT_THAT(repl.execute("() -> ()"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(),
                  core::ParameterType(repl.type_system(), {}), {}))));

  EXPECT_THAT(repl.execute("() -> i32"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(),
                  core::ParameterType(repl.type_system(), {}), {I(32)}))));

  EXPECT_THAT(
      repl.execute("() -> (i32, bool)"),
      EvaluatesTo(core::Type(core::FunctionType(
          repl.type_system(), core::ParameterType(repl.type_system(), {}),
          {I(32), Bool}))));
}

TEST(FunctionType, EvaluationWithUnnamedParameter) {
  test::Repl repl;

  core::Parameters<core::Type> parameters;
  parameters.append("", U(32));
  core::ParameterType parameter_type(repl.type_system(), std::move(parameters));

  EXPECT_THAT(repl.execute("u32 -> ()"),
              EvaluatesTo(core::Type(
                  core::FunctionType(repl.type_system(), parameter_type, {}))));

  EXPECT_THAT(repl.execute("u32 -> i32"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(), parameter_type, {I(32)}))));

  EXPECT_THAT(repl.execute("u32 -> (i32, bool)"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(), parameter_type, {I(32), Bool}))));
}

TEST(FunctionType, EvaluationWithNamedParameter) {
  test::Repl repl;

  core::Parameters<core::Type> parameters;
  parameters.append("", U(32));
  parameters.append("b", Bool);
  core::ParameterType parameter_type(repl.type_system(), std::move(parameters));

  EXPECT_THAT(repl.execute("(u32, b: bool) -> ()"),
              EvaluatesTo(core::Type(
                  core::FunctionType(repl.type_system(), parameter_type, {}))));

  EXPECT_THAT(repl.execute("(u32, b: bool) -> i32"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(), parameter_type, {I(32)}))));

  EXPECT_THAT(repl.execute("(u32, b: bool) -> (i32, bool)"),
              EvaluatesTo(core::Type(core::FunctionType(
                  repl.type_system(), parameter_type, {I(32), Bool}))));
}

}  // namespace
}  // namespace semantic_analysis
