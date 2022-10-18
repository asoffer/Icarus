#include "gtest/gtest.h"
#include "ir/value/module_id.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Builtin, Evaluation) {
  test::Repl repl;

  EXPECT_EQ(repl.execute<ir::ModuleId>("builtin"), ir::ModuleId::Builtin());
}

}  // namespace
}  // namespace semantic_analysis
