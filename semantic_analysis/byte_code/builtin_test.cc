#include "gtest/gtest.h"
#include "data_types/module_id.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Builtin, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<data_types::ModuleId>("builtin"), data_types::ModuleId::Builtin());
}

}  // namespace
}  // namespace semantic_analysis
