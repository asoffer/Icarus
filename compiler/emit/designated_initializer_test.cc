#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

std::string Context() {
  return R"(
  Int ::= struct { n := 3 }
  Pair ::= struct { a: i64 \\ b: bool }
  Wrap ::= struct (T ::= i64) { x: T }

  f ::= () -> (i64, bool) { return 3, true }
  )";
}

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({
        test::TestCase{.context  = Context(),
                       .expr     = R"(Int.{}.n)",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Int.{n = 4}.n)",
                       .expected = ir::Value(int64_t{4})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{}.a)",
                       .expected = ir::Value(int64_t{0})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{}.b)",
                       .expected = ir::Value(false)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{a = 3}.a)",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{a = 3}.b)",
                       .expected = ir::Value(false)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{b = true}.a)",
                       .expected = ir::Value(int64_t{0})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{b = true}.b)",
                       .expected = ir::Value(true)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{a = 3 \\ b = true}.a)",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Pair.{a = 3 \\ b = true}.b)",
                       .expected = ir::Value(true)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(i64).{}.x)",
                       .expected = ir::Value(int64_t{0})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(i64).{x = 3}.x)",
                       .expected = ir::Value(int64_t{3})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(bool).{}.x)",
                       .expected = ir::Value(false)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(bool).{x = true}.x)",
                       .expected = ir::Value(true)},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(f64).{}.x)",
                       .expected = ir::Value(double{0})},
        test::TestCase{.context  = Context(),
                       .expr     = R"(Wrap(f64).{x = 3.1}.x)",
                       .expected = ir::Value(3.1)},

        // TODO: Enable these tests once you allow simultaneous assignments.
        // test::TestCase{.context=Context(),.expr     = R"(Pair.{ (a, b) = 'f
        // }.a)",
        //          .expected = ir::Value(int64_t{3})},
        // test::TestCase{.context=Context(),.expr     = R"(Pair.{ (a, b) = 'f
        // }.b)",
        //          .expected = ir::Value(true)},
    }));

}  // namespace
}  // namespace compiler
