#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

TEST(PatternMatch, Success) {
  test::CompilerInfrastructure infra;
  auto& mod     = infra.add_module(R"(true ~ `B)");
  auto const* p = mod.get<ast::PatternMatch>();
  ASSERT_THAT(mod.context().qual_types(p),
              ElementsAre(type::QualType::Constant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace compiler
