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
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(1, 2))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(1, 1))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(2, 1))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(1, 2))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(1, 1))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(2, 1))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x == y)(1, 2))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x == y)(1, 1))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x != y)(2, 1))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x != y)(1, 1))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(1, 2))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(1, 1))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(2, 1))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(1, 2))",
                       .expected = ir::Value(false)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(1, 1))",
                       .expected = ir::Value(true)},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(2, 1))",
                       .expected = ir::Value(true)},

        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y < z)(1, 2, 3))",
            .expected = ir::Value(true)},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y < z)(1, 2, 1))",
            .expected = ir::Value(false)},
        test::TestCase{
            .expr =
                R"(((x: i64, y: i64, z: i64) => x < y <= z)(1, 2, 2))",
            .expected = ir::Value(true)},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(2, 3, 1))",
            .expected = ir::Value(true)},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(1, 2, 1))",
            .expected = ir::Value(true)},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(2, 3, 4))",
            .expected = ir::Value(false)},
    }));

// TODO: Add a test that covers buffer pointer comparisons.

}  // namespace
}  // namespace compiler
