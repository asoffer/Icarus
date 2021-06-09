#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string description;
  std::string expr;
  ir::Value expected;
};

using CallTest = testing::TestWithParam<TestCase>;
TEST_P(CallTest, Call) {
  auto const &[description, expr, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(R"(
  identity ::= (x: ~`T) => x
  )");

  auto const *e = mod.Append<ast::Expression>(expr);
  auto t        = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

INSTANTIATE_TEST_SUITE_P(
    All, CallTest,
    testing::ValuesIn({
        TestCase{.expr     = R"(bytes(i64))",
                 .expected = ir::Value(uint64_t{sizeof(int64_t)})},
        TestCase{.expr     = R"(alignment(i64))",
                 .expected = ir::Value(uint64_t{alignof(int64_t)})},
        TestCase{.expr     = R"(callable(i64))",
                 .expected = ir::Value(interface::Interface::Callable(
                     core::Arguments<type::Type>({type::I64}, {})))},
        TestCase{.expr     = R"(callable(bool, n = i64))",
                 .expected = ir::Value(interface::Interface::Callable(
                     core::Arguments<type::Type>({type::Bool},
                                                 {{"n", type::I64}})))},
        // TODO: Test for opaque, foreign.

        TestCase{.expr = R"((() => 3)())", .expected = ir::Value(int64_t{3})},

        TestCase{.expr     = R"(((n: i64) => n * n)(3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n: i64) => n * n)(n = 3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)(3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)(n = 3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n := 2) => n * n)())",
                 .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((n :: i64) => n * n)(3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n :: i64) => n * n)(n = 3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n ::= 2) => n * n)(3))",
                 .expected = ir::Value(int64_t{9})},
        TestCase{.expr     = R"(((n ::= 2) => n * n)(n = 3))",
                 .expected = ir::Value(int64_t{9})},
        // TestCase{.expr     = R"(((n ::= 2) => n * n)())",
        //          .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((a: i64, b: i64) => a + 2 * b)(1, 2))",
                 .expected = ir::Value(int64_t{5})},
        TestCase{.expr     = R"(((a: i64, b: i64) => a + 2 * b)(a = 1, b = 2))",
                 .expected = ir::Value(int64_t{5})},
        TestCase{.expr     = R"(((a: i64, b: i64) => a + 2 * b)(b = 1, a = 2))",
                 .expected = ir::Value(int64_t{4})},

        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1, a = 2))",
                 .expected = ir::Value(int64_t{4})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(a = 2))",
                 .expected = ir::Value(int64_t{6})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1))",
                 .expected = ir::Value(int64_t{3})},
        TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(2))",
                 .expected = ir::Value(int64_t{6})},

        TestCase{.expr     = R"(((a: ~`T) => a * a)(2))",
                 .expected = ir::Value(int64_t{4})},
        TestCase{.expr     = R"(((a: ~`T) => a * a)(2.5))",
                 .expected = ir::Value(6.25)},

        // TODO: Calling overload sets.
        TestCase{.description = "Instantiate the same generic more than once.",
                 .expr        = R"((identity(2) as f64) + identity(1.0))",
                 .expected    = ir::Value(3.0)},

        // Value to pointer casts
        TestCase{
            .expr     = R"(((n: *i64) => @n * @n)(3))",
            .expected = ir::Value(int64_t{9}),
        },
        TestCase{
            .expr     = R"(() -> i64 {
                             m := 3
                             return ((n: *i64) => @n * @n)(m)
                         }())",
            .expected = ir::Value(int64_t{9}),
        },
        TestCase{
            .expr     = R"((() -> i64 {
                             (a, b) := () -> (i64, i64) {
                               return 3, 9
                             }()
                             return a + b
                           })())",
            .expected = ir::Value(int64_t{12}),
        },

        // TODO: Value to pointer casts with structs and with designated
        // initializers.
        TestCase{
            .expr     = R"(((n: *i64) => @n * @n)(3))",
            .expected = ir::Value(int64_t{9}),
        },
    }));

}  // namespace

extern "C" {

int64_t *ForeignFunctionPtr() {
  static int64_t ptr;
  return &ptr;
}

int8_t ForeignFunctionI8() { return 17; }
int64_t ForeignFunctionI64() { return 17; }
} 

namespace {

TEST(CallTest, Foreign) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f_ptr ::= foreign("ForeignFunctionPtr", () -> *i64)
  f_i8  ::= foreign("ForeignFunctionI8", () -> i8)
  f_i64 ::= foreign("ForeignFunctionI64", () -> i64)
  )");

  {
    auto const *e = mod.Append<ast::Expression>("f_ptr()");
    auto t        = mod.context().qual_types(e)[0].type();
    ASSERT_TRUE(t.valid());
    auto result =
        mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
    ASSERT_TRUE(result);
    EXPECT_EQ(*result, ir::Value(ir::Addr(ForeignFunctionPtr())));
  }

  {
    auto const *e = mod.Append<ast::Expression>("f_i8()");
    auto t        = mod.context().qual_types(e)[0].type();
    ASSERT_TRUE(t.valid());
    auto result =
        mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
    ASSERT_TRUE(result);
    EXPECT_EQ(*result, ir::Value(ForeignFunctionI8()));
  }

  {
    auto const *e = mod.Append<ast::Expression>("f_i64()");
    auto t        = mod.context().qual_types(e)[0].type();
    ASSERT_TRUE(t.valid());
    auto result =
        mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
    ASSERT_TRUE(result);
    EXPECT_EQ(*result, ir::Value(ForeignFunctionI64()));
  }
}

}  // namespace
}  // namespace compiler
