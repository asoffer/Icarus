#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;

TEST(Builtin, Type) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(builtin)"),
              AllOf(HasQualTypes(Constant(Module)), HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis
