#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(FunctionType, EvaluationWithoutParameters) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<core::Type>("() -> ()"),
            core::FunctionType(GlobalTypeSystem,
                               core::ParameterType(GlobalTypeSystem, {}), {}));

  EXPECT_EQ(
      repl.execute<core::Type>("() -> i32"),
      core::FunctionType(GlobalTypeSystem,
                         core::ParameterType(GlobalTypeSystem, {}), {I(32)}));

  EXPECT_THAT(repl.execute<core::Type>("() -> (i32, bool)"),
              core::FunctionType(GlobalTypeSystem,
                                 core::ParameterType(GlobalTypeSystem, {}),
                                 {I(32), Bool}));
}

TEST(FunctionType, EvaluationWithUnnamedParameter) {
  test::Repl repl;

  core::Parameters<core::Type> parameters;
  parameters.append("", U(32));
  core::ParameterType parameter_type(GlobalTypeSystem, std::move(parameters));

  EXPECT_EQ(repl.execute<core::Type>("u32 -> ()"),
            core::FunctionType(GlobalTypeSystem, parameter_type, {}));

  EXPECT_EQ(repl.execute<core::Type>("u32 -> i32"),
            core::FunctionType(GlobalTypeSystem, parameter_type, {I(32)}));

  EXPECT_EQ(
      repl.execute<core::Type>("u32 -> (i32, bool)"),
      core::FunctionType(GlobalTypeSystem, parameter_type, {I(32), Bool}));
}

TEST(FunctionType, EvaluationWithNamedParameter) {
  test::Repl repl;

  core::Parameters<core::Type> parameters;
  parameters.append("", U(32));
  parameters.append("b", Bool);
  core::ParameterType parameter_type(GlobalTypeSystem, std::move(parameters));

  EXPECT_EQ(repl.execute<core::Type>("(u32, b: bool) -> ()"),
            core::FunctionType(GlobalTypeSystem, parameter_type, {}));

  EXPECT_EQ(repl.execute<core::Type>("(u32, b: bool) -> i32"),
            core::FunctionType(GlobalTypeSystem, parameter_type, {I(32)}));

  EXPECT_EQ(
      repl.execute<core::Type>("(u32, b: bool) -> (i32, bool)"),
      core::FunctionType(GlobalTypeSystem, parameter_type, {I(32), Bool}));
}

}  // namespace
}  // namespace semantic_analysis
