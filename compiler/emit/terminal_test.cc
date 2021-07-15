#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({
        test::TestCase{.expr = "true", .expected = true},
        test::TestCase{.expr = "false", .expected = false},
        test::TestCase{.expr = "null", .expected = ir::Null()},
        test::TestCase{.expr = "u8", .expected = type::U8},
        test::TestCase{.expr = "u16", .expected = type::U16},
        test::TestCase{.expr = "u32", .expected = type::U32},
        test::TestCase{.expr = "u64", .expected = type::U64},
        test::TestCase{.expr = "i8", .expected = type::I8},
        test::TestCase{.expr = "i16", .expected = type::I16},
        test::TestCase{.expr = "i32", .expected = type::I32},
        test::TestCase{.expr = "i64", .expected = type::I64},
        test::TestCase{.expr = "f32", .expected = type::F32},
        test::TestCase{.expr = "f64", .expected = type::F64},
        test::TestCase{.expr = "bool", .expected = type::Bool},
        test::TestCase{.expr = "type", .expected = type::Type_},
        test::TestCase{.expr = "module", .expected = type::Module},
        test::TestCase{.expr = "byte", .expected = type::Byte},

        // TODO: Evaluating string literals. Requires looking at read-only
        // addresses.
        // TODO: Integers and their edge cases. Especially INT_MIN.
        // TODO: Floating point edge cases.
        // TODO: Determine how you will be allowed to specify arithmetic
        // literals other than i64 or f64.
    }));

}  // namespace
}  // namespace compiler
