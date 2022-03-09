#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{
            .expr     = R"([3; i32])",
            .expected = static_cast<type::Type>(type::Arr(3, type::I32))},
        test::TestCase{.expr     = R"([1; [2; [3; bool]]])",
                       .expected = static_cast<type::Type>(type::Arr(
                           1, type::Arr(2, type::Arr(3, type::Bool))))},
        test::TestCase{.expr     = R"([1, 2, 3; i32])",
                       .expected = static_cast<type::Type>(type::Arr(
                           1, type::Arr(2, type::Arr(3, type::I32))))},
        test::TestCase{.expr     = R"(((n: i64, t: type) -> type {
          return [n, n * n; t]
        })(3, f32)
        )",
                       .expected = static_cast<type::Type>(
                           type::Arr(3, type::Arr(9, type::F32)))},
        test::TestCase{.expr     = R"(((n: i64, t: type) -> type {
          T := copy [n, n * n; t]
          return T
        })(3, f32)
        )",
                       .expected = static_cast<type::Type>(
                           type::Arr(3, type::Arr(9, type::F32)))},
        test::TestCase{.expr     = R"([1 as u8, 2 as i64; bool])",
                       .expected = static_cast<type::Type>(
                           type::Arr(1, type::Arr(2, type::Bool)))},
        test::TestCase{.expr     = R"(((n: i64, t: type) -> type {
          T := move [n, n * n; t]
          return T
        })(3, f32)
        )",
                       .expected = static_cast<type::Type>(
                           type::Arr(3, type::Arr(9, type::F32)))},
        test::TestCase{
            .expr     = R"((() -> type {
          [3, 2, 1; i64] ~ [3, `N; `T]
          return T
        })()
        )",
            .expected = static_cast<type::Type>(type::Arr(1, type::I64))},

    }));

}  // namespace
}  // namespace compiler
