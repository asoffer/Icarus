#ifndef ICARUS_TEST_EVALUATION_H
#define ICARUS_TEST_EVALUATION_H

#include <string>

#include "ast/expression.h"
#include "ast/node.h"
#include "base/ptr_span.h"
#include "compiler/builtin_module.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "compiler/resources.h"
#include "compiler/work_graph.h"
#include "compiler/work_item.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/result_buffer.h"
#include "module/module.h"
#include "module/trivial_importer.h"
#include "test/expected_value.h"
#include "test/module.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace test {

struct TestCase {
  std::string context;
  std::string expr;
  type::Type type;
  ExpectedValue expected;
};

}  // namespace test

struct EvaluationTest : testing::TestWithParam<test::TestCase> {};
TEST_P(EvaluationTest, Test) {
  test::CompilerInfrastructure infra;
  auto const &[context, expr, type, expected] = GetParam();
  auto &module = infra.add_module(absl::StrCat(context, "\n", expr, "\n"));
  ASSERT_THAT(infra.diagnostics(), testing::IsEmpty());
  ASSERT_THAT(module.module().stmts(), testing::Not(testing::IsEmpty()));
  auto const *e = module.get<ast::Expression>();
  auto qts = module.context().qual_types(e);
  ASSERT_THAT(qts, testing::SizeIs(1));
  auto t = qts[0].type();
  ASSERT_TRUE(t.valid());
  EXPECT_THAT(test::AsType(infra.Evaluate(module, e), t),
              testing::Eq(expected));
}

#endif  // ICARUS_TEST_EVALUATION_H
