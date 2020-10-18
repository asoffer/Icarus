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

using AccessTest = testing::TestWithParam<TestCase>;
TEST_P(AccessTest, Access) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
    p: *int64
    sp: *S
  }
  )");
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.data().qual_type(e);
  ASSERT_NE(qt, nullptr);
  auto t = qt->type();
  ASSERT_NE(t, nullptr);
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(All, AccessTest,
                         testing::ValuesIn({
                             TestCase{.expr     = R"((() -> int64 {
                                s: S
                                s.n = 3
                                return s.n
                             })()
                             )",
                                      .expected = ir::Value(int64_t{3})},
                             TestCase{.expr     = R"((() -> int64 {
                                s: S
                                s.n = 3
                                s.p = &s.n
                                return @s.p
                             })()
                             )",
                                      .expected = ir::Value(int64_t{3})},
                             TestCase{.expr     = R"((() -> int64 {
                                s: S
                                s.n = 3
                                s.p = &s.n
                                s.sp = &s
                                return s.sp.n
                             })()
                             )",
                                      .expected = ir::Value(int64_t{3})},
                             TestCase{.expr     = R"((() -> int64 {
                                s: S
                                ptr := &s
                                ptr.n = 3
                                ptr.p = &ptr.n
                                ptr.sp = &s
                                return ptr.sp.n
                             })()
                             )",
                                      .expected = ir::Value(int64_t{3})},
                         }));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
