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
  auto const *qt = mod.context().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto t = qt->type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    All, ArrayTypeTest,
    testing::ValuesIn({
        TestCase{.expr     = R"([3; int32])",
                 .expected = ir::Value(
                     static_cast<type::Type>(type::Arr(3, type::Int32)))},
        TestCase{.expr     = R"([1; [2; [3; bool]]])",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(1, type::Arr(2, type::Arr(3, type::Bool)))))},
        TestCase{.expr     = R"([1, 2, 3; int32])",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(1, type::Arr(2, type::Arr(3, type::Int32)))))},
        TestCase{.expr     = R"(((n: int64, t: type) -> type {
          return [n, n * n; t]
        })(3, float32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::Float32))))},
        TestCase{.expr     = R"(((n: int64, t: type) -> type {
          T := copy [n, n * n; t]
          return T
        })(3, float32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::Float32))))},
        TestCase{.expr     = R"(((n: int64, t: type) -> type {
          T := move [n, n * n; t]
          return T
        })(3, float32)
        )",
                 .expected = ir::Value(static_cast<type::Type>(
                     type::Arr(3, type::Arr(9, type::Float32))))},

    }));

}  // namespace
}  // namespace compiler
