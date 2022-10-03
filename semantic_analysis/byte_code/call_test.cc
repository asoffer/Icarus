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

TEST(Call, BuiltinForeign) {
  test::Repl repl;

  EXPECT_THAT(repl.execute(R"(builtin.foreign("MyFunction", i32 -> i32))"),
              EvaluatesTo(ir::Fn(ir::ModuleId::Builtin(), ir::LocalFnId(0))));
  EXPECT_EQ(reinterpret_cast<decltype(&MyFunction)>(
                repl.builtin_module().ForeignFunction(ir::LocalFnId(0))),
            &MyFunction);
}

}  // namespace
}  // namespace semantic_analysis
