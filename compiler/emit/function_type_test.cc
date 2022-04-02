#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/evaluation.h"
#include "type/primitive.h"

namespace compiler {
namespace {

INSTANTIATE_TEST_SUITE_P(
    All, EvaluationTest,
    testing::ValuesIn({
        test::TestCase{.expr     = R"(i32 -> bool)",
                       .expected = type::Type(type::Func(
                           {core::AnonymousParameter(
                               type::QualType::NonConstant(type::I32))},
                           {type::Bool}))},
        test::TestCase{.expr     = R"(() -> bool)",
                       .expected = type::Type(type::Func({}, {type::Bool}))},
        test::TestCase{.expr     = R"(i32 -> ())",
                       .expected = type::Type(type::Func(
                           {core::AnonymousParameter(
                               type::QualType::NonConstant(type::I32))},
                           {}))},
        test::TestCase{.expr     = R"(() -> ())",
                       .expected = type::Type(type::Func({}, {}))},
        test::TestCase{
            .expr     = R"((n: i32) -> ())",
            .expected = type::Type(type::Func(
                {core::Parameter<type::QualType>{
                    .name  = "n",
                    .value = type::QualType::NonConstant(type::I32)}},
                {}))},
        test::TestCase{
            .expr     = R"((n: i32, bool, m: u64) -> ())",
            .expected = type::Type(type::Func(
                {core::Parameter<type::QualType>{
                     .name  = "n",
                     .value = type::QualType::NonConstant(type::I32)},
                 core::AnonymousParameter(
                     type::QualType::NonConstant(type::Bool)),
                 core::Parameter<type::QualType>{
                     .name  = "m",
                     .value = type::QualType::NonConstant(type::U64)}},
                {}))},
        test::TestCase{
            .expr     = R"((n: i32, bool, m: u64) -> (i32, bool))",
            .expected = type::Type(type::Func(
                {core::Parameter<type::QualType>{
                     .name  = "n",
                     .value = type::QualType::NonConstant(type::I32)},
                 core::AnonymousParameter(
                     type::QualType::NonConstant(type::Bool)),
                 core::Parameter<type::QualType>{
                     .name  = "m",
                     .value = type::QualType::NonConstant(type::U64)}},
                {type::I32, type::Bool}))},
    }));
// TODO: Support constants, generics, defaults, and inferred types.

}  // namespace
}  // namespace compiler
