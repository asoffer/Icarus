#include "gtest/gtest.h"
#include "module/module_index.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Builtin, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<module::ModuleIndex>("builtin"),
            module::ModuleIndex::Builtin());
}

}  // namespace
}  // namespace semantic_analysis
