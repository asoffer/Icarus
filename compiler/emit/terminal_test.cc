#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string terminal;
  ir::Value expected;
};

using TerminalTest = testing::TestWithParam<TestCase>;
TEST_P(TerminalTest, Terminal) {
  auto const &[terminal, expected] = GetParam();
  test::TestModule mod;
  auto const *term = mod.Append<ast::Terminal>(terminal);
  auto result      = mod.compiler.Evaluate(type::Typed<ast::Expression const *>(
      term, mod.context().qual_type(term)->type()));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(
    All, TerminalTest,
    testing::ValuesIn({
        TestCase{.terminal = "true", .expected = ir::Value(true)},
        TestCase{.terminal = "false", .expected = ir::Value(false)},
        TestCase{.terminal = R"("")", .expected = ir::Value(ir::String(""))},
        TestCase{.terminal = R"("abc")",
                 .expected = ir::Value(ir::String("abc"))},
        TestCase{.terminal = R"("ab\"c")",
                 .expected = ir::Value(ir::String("ab\"c"))},
        TestCase{.terminal = R"("ab\n\r\t\v\\c")",
                 .expected = ir::Value(ir::String("ab\n\r\t\v\\c"))},
        TestCase{.terminal = "null", .expected = ir::Value(ir::Addr::Null())},
        TestCase{.terminal = "nat8", .expected = ir::Value(type::Nat8)},
        TestCase{.terminal = "nat16", .expected = ir::Value(type::Nat16)},
        TestCase{.terminal = "nat32", .expected = ir::Value(type::Nat32)},
        TestCase{.terminal = "nat64", .expected = ir::Value(type::Nat64)},
        TestCase{.terminal = "int8", .expected = ir::Value(type::Int8)},
        TestCase{.terminal = "int16", .expected = ir::Value(type::Int16)},
        TestCase{.terminal = "int32", .expected = ir::Value(type::Int32)},
        TestCase{.terminal = "int64", .expected = ir::Value(type::Int64)},
        TestCase{.terminal = "float32", .expected = ir::Value(type::Float32)},
        TestCase{.terminal = "float64", .expected = ir::Value(type::Float64)},
        TestCase{.terminal = "bool", .expected = ir::Value(type::Bool)},
        TestCase{.terminal = "type", .expected = ir::Value(type::Type_)},
        TestCase{.terminal = "module", .expected = ir::Value(type::Module)},
        TestCase{.terminal = "byte_view",
                 .expected = ir::Value(type::ByteView)},

        // TODO: Integers and their edge cases. Especially INT_MIN.
        // TODO: Floating point edge cases.
        // TODO: Determine how you will be allowed to specify arithmetic
        // literals other than int64 or float64.
    }));

}  // namespace
}  // namespace compiler
