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
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  ir::Block *b = result->get_if<ir::Block>();
  ASSERT_NE(b, nullptr);
  EXPECT_THAT(ir::CompiledBlock::From(*b)->after(), SizeIs(2));
  // TODO: Test `before` overload set.
}

}  // namespace
}  // namespace compiler
