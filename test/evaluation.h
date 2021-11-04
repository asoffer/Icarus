#ifndef ICARUS_TEST_EVALUATION_H
#define ICARUS_TEST_EVALUATION_H

#include <functional>
#include <string>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/result_buffer.h"
#include "test/expected_value.h"
#include "test/module.h"
#include "type/primitive.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace test {

struct TestCase {
  std::string context;
  std::string expr;
  type::Type type;
  ExpectedValue expected;
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

  compiler::Compiler c(&mod.context(), compiler::PersistentResources{
                                           .module              = &mod,
                                           .diagnostic_consumer = &mod.consumer,
                                           .importer            = &mod.importer,
                                       });
  c.set_work_resources(mod.work_resources());

  auto result = c.Evaluate(type::Typed<ast::Expression const *>(e, type));
  ASSERT_TRUE(result);
  EXPECT_EQ(expected, *result);
}

}  // namespace test

#endif  // ICARUS_TEST_EVALUATION_H
