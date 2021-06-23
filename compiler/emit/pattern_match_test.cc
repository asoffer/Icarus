#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(All, Test,
                         testing::ValuesIn({
                             test::TestCase{.context  = "20 ~ `N",
                                            .expr     = "N",
                                            .expected = ir::Value(int64_t{20})},
                             test::TestCase{.context  = "true ~ `B",
                                            .expr     = "B",
                                            .expected = ir::Value(true)},
                             test::TestCase{.context  = "23 ~ 3 + 4 * `N",
                                            .expr     = "N",
                                            .expected = ir::Value(int64_t{5})},
                         }));

}  // namespace
}  // namespace compiler
