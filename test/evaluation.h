#ifndef ICARUS_TEST_EVALUATION_H
#define ICARUS_TEST_EVALUATION_H

#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace test {

struct TestCase {
  std::string context;
  std::string expr;
  type::Type type;
  ir::Value expected;
};

using EvaluationTest = testing::TestWithParam<TestCase>;

TEST_P(EvaluationTest, Test) {
  auto const &[context, expr, type, expected] = GetParam();
  test::TestModule mod;
  mod.AppendCode(context);
  auto const *e = mod.Append<ast::Expression>(expr);
  auto qts      = mod.context().qual_types(e);
  ASSERT_EQ(qts.size(), 1);
  ASSERT_EQ(qts[0].type(), type);
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, type));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, expected);
}

}  // namespace test

#endif  // ICARUS_TEST_EVALUATION_H
