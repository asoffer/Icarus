#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

extern "C" {
int32_t MyFunction(int32_t n) { return n * n; }
}

namespace semantic_analysis {
namespace {

TEST(Call, BuiltinForeign) {
  test::Repl repl;

  auto result = repl.execute<IrFunction const *>(
      R"(builtin.foreign("MyFunction", i32 -> i32))");

  // Result needs to be computed before we look up `fn` in the foreign function
  // map, since it is populated during execution.
  EXPECT_EQ(
      reinterpret_cast<decltype(&MyFunction)>(
          repl.foreign_function_map().ForeignFunctionPointer(ir::LocalFnId(0))),
      &MyFunction);

  auto const *fn =
      repl.foreign_function_map().ForeignFunction(ir::LocalFnId(0));
  EXPECT_EQ(result, fn);

  int32_t value;
  jasmin::Execute(*fn, {int32_t{7}}, value);
  EXPECT_EQ(value, MyFunction(7));
}

}  // namespace
}  // namespace semantic_analysis
