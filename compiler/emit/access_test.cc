#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

std::string Context() {
  return R"(
  S ::= struct {
    n: i64
    p: *i64
    sp: *S
  }
)";
}

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s: S
                               s.n = 3
                               return s.n
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s: S
                               s.n = 3
                               s.p = &s.n
                               return @s.p
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s: S
                               s.n = 3
                               s.p = &s.n
                               s.sp = &s
                               return s.sp.n
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s: S
                               ptr := &s
                               ptr.n = 3
                               ptr.p = &ptr.n
                               ptr.sp = &s
                               return ptr.sp.n
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.expr     = R"((() -> u64 {
                               return "abc".length
                             })()
                             )",
                       .type     = type::I64,
                       .expected = uint64_t{3}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s := S.{n = 3}
                               x := copy s.n
                               return x
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s := S.{n = 3}
                               x := move s.n
                               return x
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{3}},
        test::TestCase{.context = Context(),
                       .expr    = R"((() -> i64 {
                               s := S.{n = 3}
                               p := &s
                               return p.n * p.n
                             })()
                             )",
                       // Loading pointer from a parameter
                       .type     = type::I64,
                       .expected = int64_t{9}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((() -> i64 {
                               s := S.{n = 3}
                               f ::= (p: *S) => p.n * p.n
                               return f(&s)
                             })()
                             )",
                       .type     = type::I64,
                       .expected = int64_t{9}},
        test::TestCase{.expr     = R"([3; i64].length)",
                       .type     = type::Integer,
                       .expected = type::Array::length_t{3}},
        test::TestCase{.expr     = R"([4, 3; i64].length)",
                       .type     = type::Integer,
                       .expected = type::Array::length_t{4}},
        test::TestCase{.expr     = R"([4, 3; i64].element_type)",
                       .type     = type::Type_,
                       .expected = type::Type(type::Arr(3, type::I64))},
        test::TestCase{.expr     = R"([4, 3; i64].element_type.element_type)",
                       .type     = type::Type_,
                       .expected = type::Type(type::I64)},
    }));

// TODO: Add a test that covers pointer parameters.

}  // namespace
}  // namespace compiler
