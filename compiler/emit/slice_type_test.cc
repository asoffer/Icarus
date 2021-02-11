#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string expr;
  ir::Value expected;
};

using SliceTypeTest = testing::TestWithParam<TestCase>;
TEST_P(SliceTypeTest, SliceLiteral) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto t        = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(
    All, SliceTypeTest,
    testing::ValuesIn({
        TestCase{.expr     = R"(i32[])",
                 .expected = ir::Value(type::Type(type::Slc(type::I32)))},
        TestCase{.expr = R"(bool[][])",
                 .expected =
                     ir::Value(type::Type(type::Slc(type::Slc(type::Bool))))},
        TestCase{.expr     = R"(((t: type) -> type {
          return t[]
        })(f32)
        )",
                 .expected = ir::Value(type::Type(type::Slc(type::F32)))},
    }));

}  // namespace
}  // namespace compiler
