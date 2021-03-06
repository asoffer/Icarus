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

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({test::TestCase{.expr     = R"(3 as u64)",
                                      .expected = ir::Value(uint64_t{3})},
                       test::TestCase{.expr     = R"(3 as i64)",
                                      .expected = ir::Value(int64_t{3})},
                       test::TestCase{.expr     = R"(3 as f64)",
                                      .expected = ir::Value(double{3})},
                       test::TestCase{.expr     = R"(3 as i16)",
                                      .expected = ir::Value(int16_t{3})},
                       test::TestCase{.expr     = R"(null as *i64)",
                                      .expected = ir::Value(ir::Null())},
                       test::TestCase{.expr     = R"(null as [*]i64)",
                                      .expected = ir::Value(ir::Null())},
                       test::TestCase{.context  = Context(),
                                      .expr     = R"(E.A as i64)",
                                      .expected = ir::Value(int64_t{1})},
                       test::TestCase{.context  = Context(),
                                      .expr     = R"((F.A | F.B) as u64)",
                                      .expected = ir::Value(uint64_t{3})},
                       test::TestCase{.expr     = R"(65 as u8 as char)",
                                      .expected = ir::Value(ir::Char('A'))},
                       test::TestCase{.expr     = R"("A"[0] as i8)",
                                      .expected = ir::Value(int8_t{65})},
                       test::TestCase{.expr     = R"("A"[0] as u8)",
                                      .expected = ir::Value(uint8_t{65})},
                       test::TestCase{.expr     = R"("A"[0] as u32)",
                                      .expected = ir::Value(uint32_t{65})}}));

// TODO: Test casting from an integer into the enum/flags

}  // namespace
}  // namespace compiler
