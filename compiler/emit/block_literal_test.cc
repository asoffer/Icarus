#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::SizeIs;

TEST(BlockLiteral, Basic) {
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(R"(block {
    before ::= () -> () {}
    after ::= jump (n: i64) { goto done() }
    after ::= jump() { goto done() }
  }
  )");
  auto t         = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  Compiler c(&mod.context(), mod.resources());
  c.set_work_resources(mod.work_resources());
  auto result =
      c.EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_THAT(ir::CompiledBlock::From(result->get<ir::Block>(0))->after(),
              SizeIs(2));

  // TODO: Test `before` overload set.
}

}  // namespace
}  // namespace compiler
