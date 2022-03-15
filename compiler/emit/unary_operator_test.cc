#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.expr = "move 3", .expected = int64_t{3}},
        test::TestCase{.context  = "f ::= () => 3",
                       .expr     = "move f()",
                       .expected = int64_t{3}},
        test::TestCase{.expr = "copy 3", .expected = int64_t{3}},
        // TODO: Test move/copy for non-trivial types.
        test::TestCase{.context  = "f ::= () => 3",
                       .expr     = "copy f()",
                       .expected = int64_t{3}},
        test::TestCase{
            .expr     = "[*]i32",
            .expected = static_cast<type::Type>(type::BufPtr(type::I32))},
        test::TestCase{
            .context  = "f ::= () => bool",
            .expr     = "[*]f()",
            .expected = static_cast<type::Type>(type::BufPtr(type::Bool))},
        test::TestCase{.expr = "not true", .expected = false},
        test::TestCase{.context  = "f ::= () => false",
                       .expr     = "not f()",
                       .expected = true},
        // TODO: Test flag negation. The problem is is we randomize the flag
        // values so we need a way to extract specific values. Perhaps this is
        // better done outside this parameterized test.
        test::TestCase{.expr = "-(3 as i8)", .expected = int8_t{-3}},
        test::TestCase{.context  = "f ::= () => 3 as i8",
                       .expr     = "-f()",
                       .expected = int8_t{-3}},
        test::TestCase{.expr = "-(3 as i16)", .expected = int16_t{-3}},
        test::TestCase{.context  = "f ::= () => 3 as i16",
                       .expr     = "-f()",
                       .expected = int16_t{-3}},
        test::TestCase{.expr = "-(3 as i32)", .expected = int32_t{-3}},
        test::TestCase{.context  = "f ::= () => 3 as i32",
                       .expr     = "-f()",
                       .expected = int32_t{-3}},
        test::TestCase{.expr = "-(3 as i64)", .expected = int64_t{-3}},
        test::TestCase{.context  = "f ::= () => 3 as i64",
                       .expr     = "-f()",
                       .expected = int64_t{-3}},
        test::TestCase{.expr = "-(3.0 as f32)", .expected = float{-3}},
        test::TestCase{.context  = "f ::= () => 3 as f32",
                       .expr     = "-f()",
                       .expected = float{-3}},
        test::TestCase{.expr = "-(3.0 as f32)", .expected = float{-3}},
        test::TestCase{.context  = "f ::= () => 3 as f32",
                       .expr     = "-f()",
                       .expected = float{-3}},
        test::TestCase{.expr = "-(3.0 as f64)", .expected = double{-3}},
        test::TestCase{.context  = "f ::= () => 3 as f64",
                       .expr     = "-f()",
                       .expected = double{-3}},
        test::TestCase{.expr     = "true:?",
                       .expected = static_cast<type::Type>(type::Bool)},
        test::TestCase{.context  = "f ::= () => true",
                       .expr     = "f():?",
                       .expected = static_cast<type::Type>(type::Bool)},
        test::TestCase{
            .expr     = "*i32",
            .expected = static_cast<type::Type>(type::Ptr(type::I32))},
        test::TestCase{
            .context  = "f ::= () => i32",
            .expr     = "*f()",
            .expected = static_cast<type::Type>(type::Ptr(type::I32))},
        test::TestCase{.context  = R"(
               f ::= () -> i64 {
                 n: i64
                 np := &n
                 n = 3
                 return @np
               }
               )",
                       .expr     = "f()",
                       .expected = int64_t{3}},
        // TODO: Fix pattern matching.
        // test::TestCase{.context  = R"(*[*]i64 ~ *`T)",
        //                .expr     = "T",
        //                .expected = type::Type(type::BufPtr(type::I64))},
        // test::TestCase{.context  = R"([*]*i64 ~ [*]`T)",
        //                .expr     = "T",
        //                .expected = type::Type(type::Ptr(type::I64))},
        // test::TestCase{
        //     .context = R"(3 ~ -`N)", .expr = "N", .expected = int64_t{-3}},
        // test::TestCase{
        //     .context = R"(3.1 ~ -`N)", .expr = "N", .expected = -3.1},
        // test::TestCase{
        //     .context = R"(true ~ not `B)", .expr = "B", .expected = false},
    }));

}  // namespace
}  // namespace compiler
