#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::Eq;
using ::testing::Optional;

std::string Context() {
  return R"(
  identity ::= (x: ~`T) => x
  )";
}

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.expr     = R"(builtin.bytes(i64))",
                       .expected = uint64_t{sizeof(int64_t)}},
        test::TestCase{.expr     = R"(builtin.alignment(i64))",
                       .expected = uint64_t{alignof(int64_t)}},
        // TODO: Test for opaque, foreign.
        test::TestCase{.expr = R"((() => 3)())", .expected = int64_t{3}},

        test::TestCase{.expr     = R"(((n: i64) => n * n)(3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n: i64) => n * n)(n = 3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n := 2) => n * n)(3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n := 2) => n * n)(n = 3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n := 2) => n * n)())",
                       .expected = int64_t{4}},

        test::TestCase{.expr     = R"(((n :: i64) => n * n)(3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n :: i64) => n * n)(n = 3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n ::= 2) => n * n)(3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n ::= 2) => n * n)(n = 3))",
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"(((n ::= 2) => n * n)())",
                       .expected = int64_t{4}},
        test::TestCase{.expr     = R"(((a: i64, b: i64) => a + 2 * b)(1, 2))",
                       .expected = int64_t{5}},
        test::TestCase{
            .expr     = R"(((a: i64, b: i64) => a + 2 * b)(a = 1, b = 2))",
            .expected = int64_t{5}},
        test::TestCase{
            .expr     = R"(((a: i64, b: i64) => a + 2 * b)(b = 1, a = 2))",
            .expected = int64_t{4}},

        test::TestCase{
            .expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1, a = 2))",
            .expected = int64_t{4}},
        test::TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(a = 2))",
                       .expected = int64_t{6}},
        test::TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(b = 1))",
                       .expected = int64_t{3}},
        test::TestCase{.expr     = R"(((a := 1, b := 2) => a + 2 * b)(2))",
                       .expected = int64_t{6}},

        test::TestCase{.expr     = R"(((a: ~`T) => a * a)(2))",
                       .expected = int64_t{4}},
        test::TestCase{.expr = R"(((a: ~`T) => a * a)(2.5))", .expected = 6.25},

        // TODO: Calling overload sets.

        test::TestCase{// "Instantiate the same generic more than once."
                       .context  = Context(),
                       .expr     = R"((identity(2) as f64) + identity(1.0))",
                       .expected = 3.0},

        // Value to pointer casts
        test::TestCase{
            .expr     = R"(((n: *i64) => @n * @n)(3 as i64))",
            .expected = int64_t{9},
        },
        test::TestCase{
            .expr     = R"(() -> i64 {
                             m := 3
                             return ((n: *i64) => @n * @n)(m)
                         }())",
            .expected = int64_t{9},
        },
        test::TestCase{
            .expr     = R"((() -> i64 {
                             (a, b) := () -> (i64, i64) {
                               return 3, 9
                             }()
                             return a + b
                           })())",
            .expected = int64_t{12},
        },

        // TODO: Value to pointer casts with structs and with designated
        // initializers.
        test::TestCase{
            .expr     = R"(((n: *i64) => @n * @n)(3 as i64))",
            .expected = int64_t{9},
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
  constexpr std::string_view kDefinitions = R"(
  f_ptr ::= builtin.foreign("ForeignFunctionPtr", () -> *i64)
  f_i8  ::= builtin.foreign("ForeignFunctionI8", () -> i8)
  f_i64 ::= builtin.foreign("ForeignFunctionI64", () -> i64)
  )";

  {
    test::CompilerInfrastructure infra;
    auto &mod     = infra.add_module(absl::StrCat(kDefinitions, "f_ptr()"));
    auto const *e = mod.get<ast::Expression>();
    ASSERT_THAT(
        infra.Evaluate(mod, e),
        Optional(Eq(test::ExpectedValue(ir::Addr(ForeignFunctionPtr())))));
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod     = infra.add_module(absl::StrCat(kDefinitions, "f_i8()"));
    auto const *e = mod.get<ast::Expression>();
    ASSERT_THAT(infra.Evaluate(mod, e),
                Optional(Eq(test::ExpectedValue(ForeignFunctionI8()))));
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod     = infra.add_module(absl::StrCat(kDefinitions, "f_i64()"));
    auto const *e = mod.get<ast::Expression>();
    ASSERT_THAT(infra.Evaluate(mod, e),
                Optional(Eq(test::ExpectedValue(ForeignFunctionI64()))));
  }
}

}  // namespace
}  // namespace compiler
