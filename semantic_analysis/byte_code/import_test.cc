#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Import, Computation) {
  test::Repl repl;

  repl.module().insert_module("abc", data_types::ModuleId(7));
  EXPECT_EQ(repl.execute<data_types::ModuleId>(R"(import "abc")"),
            data_types::ModuleId(7));
}

}  // namespace
}  // namespace semantic_analysis
