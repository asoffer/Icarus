#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;

TEST(ShortFunctionLiteral, NoParameters) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(() => true)"),
              AllOf(HasQualTypes(Constant(core::FunctionType(
                        GlobalTypeSystem,
                        core::ParameterType(GlobalTypeSystem, {}), {Bool}))),
                    HasDiagnostics()));
}

TEST(ShortFunctionLiteral, OneParameter) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"((n: i64) => n)"),
      AllOf(HasQualTypes(Constant(core::FunctionType(
                GlobalTypeSystem,
                core::ParameterType(GlobalTypeSystem,
                                    core::Parameters<core::Type>{
                                        core::Parameter<core::Type>{
                                            .name = "n", .value = I(64)},
                                    }),
                {I(64)}))),
            HasDiagnostics()));
}

TEST(ShortFunctionLiteral, MultipleParameter) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"((n: i64, b: bool) => b)"),
      AllOf(
          HasQualTypes(Constant(core::FunctionType(
              GlobalTypeSystem,
              core::ParameterType(
                  GlobalTypeSystem,
                  core::Parameters<core::Type>{
                      core::Parameter<core::Type>{.name = "n", .value = I(64)},
                      core::Parameter<core::Type>{.name = "b", .value = Bool},
                  }),
              {Bool}))),
          HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
