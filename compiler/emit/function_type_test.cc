#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string expr;
  ir::Value expected;
};

using AccessTest = testing::TestWithParam<TestCase>;
TEST_P(AccessTest, Access) {
  auto const &[expr, expected] = GetParam();
  test::TestModule mod;
  auto const *e = mod.Append<ast::Expression>(expr);
  auto t        = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    All, AccessTest,
    testing::ValuesIn({
        TestCase{
            .expr     = R"(i32 -> bool)",
            .expected = ir::Value(type::Type(type::Func(
                {core::AnonymousParam(type::QualType::NonConstant(type::I32))},
                {type::Bool})))},
        TestCase{
            .expr     = R"(() -> bool)",
            .expected = ir::Value(type::Type(type::Func({}, {type::Bool})))},
        TestCase{
            .expr     = R"(i32 -> ())",
            .expected = ir::Value(type::Type(type::Func(
                {core::AnonymousParam(type::QualType::NonConstant(type::I32))},
                {})))},
        TestCase{.expr     = R"(() -> ())",
                 .expected = ir::Value(type::Type(type::Func({}, {})))},
        TestCase{.expr     = R"((n: i32) -> ())",
                 .expected = ir::Value(type::Type(type::Func(
                     {core::Param<type::QualType>(
                         "n", type::QualType::NonConstant(type::I32))},
                     {})))},
        TestCase{
            .expr     = R"((n: i32, bool, m: u64) -> ())",
            .expected = ir::Value(type::Type(type::Func(
                {core::Param<type::QualType>(
                     "n", type::QualType::NonConstant(type::I32)),
                 core::AnonymousParam(type::QualType::NonConstant(type::Bool)),
                 core::Param<type::QualType>(
                     "m", type::QualType::NonConstant(type::U64))},
                {})))},
        TestCase{
            .expr     = R"((n: i32, bool, m: u64) -> (i32, bool))",
            .expected = ir::Value(type::Type(type::Func(
                {core::Param<type::QualType>(
                     "n", type::QualType::NonConstant(type::I32)),
                 core::AnonymousParam(type::QualType::NonConstant(type::Bool)),
                 core::Param<type::QualType>(
                     "m", type::QualType::NonConstant(type::U64))},
                {type::I32, type::Bool})))},
    }));
// TODO: Support constants, generics, defaults, and inferred types.

}  // namespace
}  // namespace compiler
