#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({test::TestCase{.expr     = R"((() -> i64 {
                                              a ::= 1 
                                              return a
                                            })()
                                            )",
                                      .expected = int64_t{1}},
                       test::TestCase{.expr     = R"(((n :: i64) -> i64 {
                                              return n
                                            })(2)
                                            )",
                                      .expected = int64_t{2}},
                       test::TestCase{.expr     = R"((() -> i64 {
                                              a := 3
                                              return a
                                            })()
                                            )",
                                      .expected = int64_t{3}},
                       test::TestCase{.expr     = R"(((n: i64) -> i64 {
                                              return n
                                            })(4)
                                            )",
                                      .expected = int64_t{4}}}));

}  // namespace
}  // namespace compiler
