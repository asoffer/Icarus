#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

extern "C" {
int32_t MyFunction(int32_t n) { return n * n; }
} 

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;
using ::testing::_;

TEST(Call, BuiltinForeign) {
  test::Repl repl;

  auto result = repl.execute(R"(builtin.foreign("MyFunction", i32 -> i32))");

  // We need to run the evaluation (which happens inside the matcher) in order
  // to populate the foreign function map.
  EXPECT_THAT(result, EvaluatesTo<IrFunction const *>(_));

  EXPECT_EQ(reinterpret_cast<decltype(&MyFunction)>(
                repl.foreign_function_map().ForeignFunctionPointer(ir::LocalFnId(0))),
            &MyFunction);
  EXPECT_THAT(
      result,
      EvaluatesTo(repl.foreign_function_map().ForeignFunction(ir::LocalFnId(0))));
}

}  // namespace
}  // namespace semantic_analysis
