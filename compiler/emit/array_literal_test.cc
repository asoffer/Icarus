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
        test::TestCase{.expr     = R"((() -> i64 { return [3][0] })())",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.expr     = R"((() -> i64 { return [1, 4, 9][2] })())",
                       .expected = ir::Value(int64_t{9})},
        test::TestCase{.expr = R"((() -> f64 { return [1.0, 4.4, 9.9][1] })())",
                       .expected = ir::Value(4.4)},
        test::TestCase{.expr     = R"((() -> i64 {
                                   a := [3]
                                   return a[0]
                                 })()
                                 )",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.expr     = R"((() -> i64 {
                                   a := [1, 4, 9]
                                   return a[2]
                                 })()
                                 )",
                       .expected = ir::Value(int64_t{9})},
        test::TestCase{.expr     = R"((() -> f64 {
                                   a := [1.0, 4.4, 9.9]
                                   return a[1]
                                 })()
                                 )",
                       .expected = ir::Value(4.4)},

        test::TestCase{.expr     = R"((() -> i64 {
                                   a := [3]
                                   return a[0]
                                 })()
                                 )",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.expr     = R"((() -> i64 {
                                   a := [1, 4, 9]
                                   return a[2]
                                 })()
                                 )",
                       .expected = ir::Value(int64_t{9})},
        test::TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                       .expected = ir::Value(4.4)},
        test::TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := copy [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                       .expected = ir::Value(4.4)},
        test::TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a: [3; f64]
                                   a = [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                       .expected = ir::Value(4.4)},
        test::TestCase{.expr     = R"(((f: f64) -> f64 {
                                   a := move [1.0, f, 9.9]
                                   return a[1]
                                 })(4.4)
                                 )",
                       .expected = ir::Value(4.4)},
    }));

}  // namespace
}  // namespace compiler
