#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string expr;
  ir::Value expected;
};

using AssignmentTest = testing::TestWithParam<TestCase>;
TEST_P(AssignmentTest, Assignment) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.context().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto t = qt->type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    All, AssignmentTest,
    testing::ValuesIn({
        TestCase{.expr     = R"(((n: i64) -> i64 {
                                  a: i64
                                  a = n
                               return a
                             })(3)
                             )",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"(((n: i64, m: i64) -> i64 {
                                 a: i64
                                 b: i64
                                 (a, b) = (n, m)
                               return 10 * a + b
                             })(1, 2)
                             )",
                 .expected = ir::Value(int64_t{12})},
        TestCase{.expr     = R"((() -> i64 {
                                a := 1
                                b := 2
                                (a, b) = (b, a)
                               return 10 * a + b
                             })()
                             )",
                 .expected = ir::Value(int64_t{21})},
        // Auto-generated assignment
        TestCase{.expr     = R"((() -> i64 {
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
        TestCase{.expr     = R"((() -> i64 {
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
        TestCase{.expr     = R"((() -> i64 {
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
                 .expected = ir::Value(int64_t{21})}
    }));

}  // namespace
}  // namespace compiler
