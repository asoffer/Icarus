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
  EXPECT_THAT(mod.context().qual_types(term),
              testing::UnorderedElementsAre(expected));
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
            .expected = type::QualType::Constant(type::Slc(type::Char)),
        },
        TestCase{
            .terminal = R"("abc")",
            .expected = type::QualType::Constant(type::Slc(type::Char)),
        },
        TestCase{
            .terminal = R"("ab\"c")",
            .expected = type::QualType::Constant(type::Slc(type::Char)),
        },
        TestCase{
            .terminal = R"("ab\n\r\t\v\\c")",
            .expected = type::QualType::Constant(type::Slc(type::Char)),
        },
        TestCase{
            .terminal = "null",
            .expected = type::QualType::Constant(type::NullPtr),
        },
        TestCase{
            .terminal = "u8",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "u16",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "u32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "u64",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "i8",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "i16",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "i32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "i64",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "f32",
            .expected = type::QualType::Constant(type::Type_),
        },
        TestCase{
            .terminal = "f64",
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
            .terminal = R"(!a)",
            .expected = type::QualType::Constant(type::Char),
        },
        TestCase{
            .terminal = R"(!\a)",
            .expected = type::QualType::Constant(type::Char),
        },
        TestCase{
            .terminal = "memory",
            .expected = type::QualType::Constant(type::Type_),
        },

        // TODO: Integers and their edge cases. Especially INT_MIN.
        // TODO: Floating point edge cases.
        // TODO: Determine how you will be allowed to specify arithmetic
        // literals other than i64 or f64.
    }));

}  // namespace
}  // namespace compiler
