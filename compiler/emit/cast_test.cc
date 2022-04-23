#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

std::string Context() {
  return R"(
  E ::= enum { A ::= 1 as u64 \\ B ::= 2 as u64 \\ C ::= 3 as u64 }
  F ::= flags { A ::= 1 as u64 \\ B ::= 2 as u64 \\ C ::= 4 as u64 }
  )";
}

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.expr = R"(3 as u64)", .expected = uint64_t{3}},
        test::TestCase{.expr = R"(3 as i64)", .expected = int64_t{3}},
        test::TestCase{.expr = R"(3 as f64)", .expected = double{3}},
        test::TestCase{.expr = R"(3 as i16)", .expected = int16_t{3}},
        test::TestCase{.expr = R"(null as *i64)", .expected = ir::Null()},
        test::TestCase{.expr = R"(null as [*]i64)", .expected = ir::Null()},
        test::TestCase{.context  = Context(),
                       .expr     = R"(E.A as u64)",
                       .expected = uint64_t{1}},
        test::TestCase{.context  = Context(),
                       .expr     = R"((F.A | F.B) as u64)",
                       .expected = uint64_t{3}},
    }));

// TODO: Test casting from an integer into the enum/flags

}  // namespace
}  // namespace compiler
