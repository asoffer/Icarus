#ifndef ICARUS_TEST_MODULE_H
#define ICARUS_TEST_MODULE_H

#include <string>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/expression.h"
#include "ast/overload_set.h"
#include "base/log.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/source/range.h"
#include "module/mock_importer.h"
#include "module/module.h"
#include "test/util.h"

namespace test {

struct TestModule : compiler::CompiledModule {
  TestModule()
      : compiler({
            .data                = context(),
            .diagnostic_consumer = consumer,
            .importer            = importer,
        }) {}
  ~TestModule() { compiler.CompleteDeferredBodies(); }

  void AppendCode(std::string code) {
    code.push_back('\n');
    frontend::StringSource source(std::move(code));
    AppendNodes(frontend::Parse(source, consumer, next_line_num), consumer);
    next_line_num += 100;
  }

  template <typename NodeType>
  NodeType const* Append(std::string code) {
    auto node       = test::ParseAs<NodeType>(std::move(code), next_line_num);
    auto const* ptr = ASSERT_NOT_NULL(node.get());
    next_line_num += 100;
    AppendNode(std::move(node), consumer);
    return ptr;
  }

  module::MockImporter importer;
  diagnostic::TrackingConsumer consumer;
  compiler::Compiler compiler;

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer& diag) override {
    compiler.VerifyAll(nodes);
    compiler.CompleteDeferredBodies();
  }

 private:
  frontend::LineNum next_line_num = frontend::LineNum(1);
};

ast::Call const* MakeCall(
    std::string fn, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args,
    TestModule* module) {
  auto call_expr = std::make_unique<ast::Call>(
      frontend::SourceRange{}, ParseAs<ast::Expression>(std::move(fn)),
      MakeArguments(std::move(pos_args), std::move(named_args)));
  auto* expr = call_expr.get();
  module->AppendNode(std::move(call_expr), module->consumer);
  return expr;
}

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
