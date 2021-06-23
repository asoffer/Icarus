#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({test::TestCase{.expr     = R"(((n: i64) -> i64 {
                                  a: i64
                                  a = n
                               return a
                             })(3)
                             )",
                                      .expected = ir::Value(int64_t{3})},
                       test::TestCase{.expr     = R"(((n: i64, m: i64) -> i64 {
                                 a: i64
                                 b: i64
                                 (a, b) = (n, m)
                               return 10 * a + b
                             })(1, 2)
                             )",
                                      .expected = ir::Value(int64_t{12})},
                       test::TestCase{.expr     = R"((() -> i64 {
                                a := 1
                                b := 2
                                (a, b) = (b, a)
                               return 10 * a + b
                             })()
                             )",
                                      .expected = ir::Value(int64_t{21})},
                       // Auto-generated assignment
                       test::TestCase{.expr     = R"((() -> i64 {
                                  S ::= struct {
                                    _a: i64
                                  }
                                  s1 := S.{ _a = 1 }
                                  s2 := S.{ _a = 2 }
                                  (s1, s2) = (s2, s1)
                               return 10 * s1._a + s2._a
                             })()
                             )",
                                      .expected = ir::Value(int64_t{21})},
                       // User-specified copy-assign
                       test::TestCase{.expr     = R"((() -> i64 {
                                  S ::= struct {
                                    _a: i64
                                    (copy) ::= (lhs: *S, rhs: *S) -> () {
                                      lhs._a = rhs._a
                                    }
                                  }
                                  s1 := S.{ _a = 1 }
                                  s2 := S.{ _a = 2 }
                                  (s1, s2) = (s2, s1)
                               return 10 * s1._a + s2._a
                             })()
                             )",
                                      .expected = ir::Value(int64_t{21})},
                       // User-specified move-assign
                       test::TestCase{.expr     = R"((() -> i64 {
                                  S ::= struct {
                                    _a: i64
                                    (move) ::= (lhs: *S, rhs: *S) -> () {
                                      lhs._a = rhs._a
                                    }
                                  }
                                  s1 := S.{ _a = 1 }
                                  s2 := S.{ _a = 2 }
                                  (s1, s2) = (s2, s1)
                               return 10 * s1._a + s2._a
                             })()
                             )",
                                      .expected = ir::Value(int64_t{21})}}));

}  // namespace
}  // namespace compiler
