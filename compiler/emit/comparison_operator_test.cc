#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(1, 3))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(1, 1))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x < y)(2, 1))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(1, 2))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(1, 1))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x <= y)(2, 1))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x == y)(1, 2))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x == y)(1, 1))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x != y)(2, 1))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x != y)(1, 1))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(1, 2))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(1, 1))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x > y)(2, 1))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(1, 2))",
                       .expected = false},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(1, 1))",
                       .expected = true},
        test::TestCase{.expr     = R"(((x: i64, y: i64) => x >= y)(2, 1))",
                       .expected = true},

        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y < z)(1, 2, 3))",
            .expected = true},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y < z)(1, 2, 1))",
            .expected = false},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y <= z)(1, 2, 2))",
            .expected = true},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(2, 3, 1))",
            .expected = true},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(1, 2, 1))",
            .expected = true},
        test::TestCase{
            .expr     = R"(((x: i64, y: i64, z: i64) => x < y > z)(2, 3, 4))",
            .expected = false},
    }));

// TODO: Add a test that covers buffer pointer comparisons.

}  // namespace
}  // namespace compiler
