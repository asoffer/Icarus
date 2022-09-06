#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/slice.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;

TEST(ShortFunctionLiteral, NoParameters) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(() => true)"),
              AllOf(HasQualTypes(
                        type::QualType::Constant(type::Func({}, {type::Bool}))),
                    HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
