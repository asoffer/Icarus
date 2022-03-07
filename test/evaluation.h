#ifndef ICARUS_TEST_EVALUATION_H
#define ICARUS_TEST_EVALUATION_H

#include <functional>
#include <string>

#include "ast/node.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "compiler/resources.h"
#include "compiler/work_graph.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "frontend/source_indexer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/result_buffer.h"
#include "module/module.h"
#include "module/trivial_importer.h"
#include "test/expected_value.h"
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
  compiler::WorkSet work_set;
  module::SharedContext shared_context;
  module::TrivialImporter importer;
  ir::Module ir_module;
  compiler::Context ctx(&ir_module);

  diagnostic::TrackingConsumer consumer;
  auto [mod_id, module] =
      shared_context.module_table().add_module<compiler::CompiledModule>("",
                                                                         &ctx);

  auto const &[context, expr, type, expected] = GetParam();

  size_t num = consumer.num_consumed();
  auto stmts =
      frontend::Parse(absl::StrCat(context, "\n", expr, "\n"), consumer);
  if (consumer.num_consumed() != num) {
    FAIL() << "Parisng failure.";
    return;
  }

  auto const *e = &stmts.back()->as<ast::Expression>();
  compiler::PersistentResources resources{
      .work                = &work_set,
      .module              = module,
      .diagnostic_consumer = &consumer,
      .importer            = &importer,
      .shared_context      = &shared_context,
  };

  compiler::WorkGraph work_graph(resources);

  compiler::CompileModule(ctx, resources,
                          module->insert(stmts.begin(), stmts.end()));

  auto qts = ctx.qual_types(e);
  ASSERT_EQ(qts.size(), 1);
  ASSERT_EQ(qts[0].type(), type);

  compiler::CompilationData data{
      .context        = &ctx,
      .work_resources = work_graph.work_resources(),
      .resources =
          {
              .work                = &work_set,
              .module              = module,
              .diagnostic_consumer = &consumer,
              .importer            = &importer,
              .shared_context      = &shared_context,
          },
  };
  compiler::Compiler c(&data);
  EXPECT_THAT(c.EvaluateToBufferOrDiagnose(
                  type::Typed<ast::Expression const *>(e, type)),
              ::testing::Optional(::testing::Eq(expected)));
}

}  // namespace test

#endif  // ICARUS_TEST_EVALUATION_H
