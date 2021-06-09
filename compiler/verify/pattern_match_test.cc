#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

TEST(PatternMatch, Success) {
  test::TestModule mod;
  auto const *p = mod.Append<ast::PatternMatch>(R"(true ~ `B)");
  ASSERT_THAT(mod.context().qual_types(p),
              ElementsAre(type::QualType::Constant(type::Bool)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace compiler
