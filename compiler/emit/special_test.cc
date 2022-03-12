#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

std::string Context() {
  return R"(
  S ::= struct {
    n:= 3 as i64
    p: *i64
  }
  )";
}

INSTANTIATE_TEST_SUITE_P(All, EvaluationTest,
                         testing::ValuesIn({
                             test::TestCase{.expr     = R"((() -> {
                               b: bool
                               return b
                             })()
                             )",
                                            .expected = false},
                             test::TestCase{.expr     = R"((() -> {
                               n: u8
                               return n
                             })()
                             )",
                                            .expected = uint8_t{0}},
                             test::TestCase{.expr     = R"((() -> {
                               n: u16
                               return n
                             })()
                             )",
                                            .expected = uint16_t{0}},

                             test::TestCase{.expr     = R"((() -> {
                               n: u32
                               return n
                             })()
                             )",
                                            .expected = uint32_t{0}},

                             test::TestCase{.expr     = R"((() -> {
                               n: u64
                               return n
                             })()
                             )",
                                            .expected = uint64_t{0}},
                             test::TestCase{.expr     = R"((() -> {
                               n: i8
                               return n
                             })()
                             )",
                                            .expected = int8_t{0}},
                             test::TestCase{.expr     = R"((() -> {
                               n: i16
                               return n
                             })()
                             )",
                                            .expected = int16_t{0}},

                             test::TestCase{.expr     = R"((() -> {
                               n: i32
                               return n
                             })()
                             )",
                                            .expected = int32_t{0}},

                             test::TestCase{.expr     = R"((() -> {
                               n: i64
                               return n
                             })()
                             )",
                                            .expected = int64_t{0}},
                             test::TestCase{.expr     = R"((() -> {
                               p: *i64
                               return p
                             })()
                             )",
                                            .expected = ir::Null()},
                             test::TestCase{.expr     = R"((() -> {
                               p: [*]bool
                               return p
                             })()
                             )",
                                            .expected = ir::Null()},

                             test::TestCase{.context  = Context(),
                                            .expr     = R"((() -> {
                               // Test struct initialization
                               s: S
                               return s.p
                             })()
                             )",
                                            .expected = ir::Null()},

                             test::TestCase{.context  = Context(),
                                            .expr     = R"((() -> {
                               // Test struct initialization
                               s: S
                               return s.n
                             })()
                             )",
                                            .expected = int64_t{3}},

                             // TODO: Tests for tuples and arrays
                             // TODO: Tests for struct destructors, including
                             //       nested in arrays, tuples or other structs.
                             // TODO: Copy/move assignment tests

                         }));

}  // namespace
}  // namespace compiler
