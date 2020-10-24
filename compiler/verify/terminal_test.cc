#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct TestCase {
  std::string terminal;
  type::QualType expected;
};

using TerminalTest = testing::TestWithParam<TestCase>;
TEST_P(TerminalTest, Terminal) {
  auto const &[terminal, expected] = GetParam();
  test::TestModule mod;
  auto const *term = mod.Append<ast::Terminal>(terminal);
  auto const *qt   = mod.context().qual_type(term);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, expected);
}

// Note: We do not need to handle type-errors here because none are possible:
// Any type error is a syntactic error that was caught by the lexer.
INSTANTIATE_TEST_SUITE_P(
    All, TerminalTest,
    testing::ValuesIn({
        TestCase{
            .terminal = "true",
            .expected = type::QualType::Constant(type::Bool),
        },
        TestCase{
            .terminal = "false",
            .expected = type::QualType::Constant(type::Bool),
        },
        TestCase{
            .terminal = R"("")",
            .expected = type::QualType::Constant(type::ByteView),
        },
        TestCase{
            .terminal = R"("abc")",
            .expected = type::QualType::Constant(type::ByteView),
        },
        TestCase{
            .terminal = R"("ab\"c")",
            .expected = type::QualType::Constant(type::ByteView),
        },
        TestCase{
            .terminal = R"("ab\n\r\t\v\\c")",
            .expected = type::QualType::Constant(type::ByteView),
        },
        TestCase{
            .terminal = "null",
            .expected = type::QualType::Constant(type::NullPtr),
        },
        TestCase{
            .terminal = "nat8",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "nat16",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "nat32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "nat64",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "int8",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "int16",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "int32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "int64",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "float32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "float64",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "bool",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "type",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "module",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "byte_view",
            .expected = type::QualType::Constant(type::Type_),
        },

        // TODO: Integers and their edge cases. Especially INT_MIN.
        // TODO: Floating point edge cases.
        // TODO: Determine how you will be allowed to specify arithmetic
        // literals other than int64 or float64.
    }));

}  // namespace
}  // namespace compiler
