#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using Test = test::EvaluationTest;
INSTANTIATE_TEST_SUITE_P(
    All, Test,
    testing::ValuesIn({
        test::TestCase{.expr     = R"([]i32)",
                       .expected = type::Type(type::Slc(type::I32))},
        test::TestCase{
            .expr     = R"([][]bool)",
            .expected = type::Type(type::Slc(type::Slc(type::Bool)))},
        test::TestCase{.expr     = R"(((t: type) -> type {
          return []t
        })(f32)
        )",
                       .expected = type::Type(type::Slc(type::F32))},
        test::TestCase{.context  = R"([]*i64 ~ []`T)",
                       .expr     = "T",
                       .expected = type::Type(type::Ptr(type::I64))},
    }));

}  // namespace
}  // namespace compiler
