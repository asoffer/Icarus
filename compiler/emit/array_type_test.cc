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

using ArrayTypeTest = testing::TestWithParam<TestCase>;
TEST_P(ArrayTypeTest, ArrayLiteral) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto t         = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(
    All, ArrayTypeTest,
    testing::ValuesIn({
        TestCase{.expr     = R"([3; i32])",
                 .expected = ir::Value(
                     static_cast<type::Type>(type::Arr(3, type::I32)))},
        TestCase{.expr     = R"([1; [2; [3; bool]]])",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(1, type::Arr(2, type::Arr(3, type::Bool)))))},
        TestCase{.expr     = R"([1, 2, 3; i32])",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(1, type::Arr(2, type::Arr(3, type::I32)))))},
        TestCase{.expr     = R"(((n: i64, t: type) -> type {
          return [n, n * n; t]
        })(3, f32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::F32))))},
        TestCase{.expr     = R"(((n: i64, t: type) -> type {
          T := copy [n, n * n; t]
          return T
        })(3, f32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::F32))))},
        TestCase{.expr     = R"([1 as u8, 2 as i64; bool])",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(1, type::Arr(2, type::Bool))))},
        TestCase{.expr     = R"(((n: i64, t: type) -> type {
          T := move [n, n * n; t]
          return T
        })(3, f32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::F32))))},

    }));

}  // namespace
}  // namespace compiler
