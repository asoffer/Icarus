#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({test::TestCase{.expr     = R"("abc"[0])",
                                      .expected = ir::Char('a')},
                       test::TestCase{.expr     = R"([1, 2, 3][0])",
                                      .expected = int64_t{1}},
                       test::TestCase{.expr     = R"((() -> i64 {
        // Reference buffer-pointer indexing.
        a := [1, 2, 3]
        p: [*]i64 = &a[1]
        return p[1]
        })()
        )",
                                      .expected = int64_t{3}},
                       test::TestCase{.expr     = R"((() -> i64 {
        // Non-reference buffer-pointer indexing.
        a := [1, 2, 3, 4]
        p: [*]i64 = &a[1]
        return (&(p[1]))[1]
        })()
        )",
                                      .expected = int64_t{4}}}));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
