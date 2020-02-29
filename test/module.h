#ifndef ICARUS_TEST_MODULE_H
#define ICARUS_TEST_MODULE_H

#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "ast/expression.h"
#include "ast/overload_set.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/source/range.h"
#include "module/module.h"
#include "test/util.h"

namespace test {

struct TestModule : compiler::CompiledModule {
  TestModule() : consumer(stderr), compiler(this, consumer) {}
  ~TestModule() { compiler.CompleteDeferredBodies(); }

  template <typename NodeType>
  NodeType const* Append(std::string code) {
    auto node       = test::ParseAs<NodeType>(std::move(code));
    auto const* ptr = node.get();
    AppendNode(std::move(node));
    return ptr;
  }

  diagnostic::StreamingConsumer consumer;
  compiler::Compiler compiler;

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer& diag) override {
    for (ast::Node const* node : nodes) {
      compiler.Visit(node, compiler::VerifyTypeTag{});
    }
    compiler.CompleteDeferredBodies();
  }
};

std::pair<ast::OverloadSet, ast::Call*> MakeCall(
    std::string fn, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args,
    TestModule* module) {
  auto call_expr = std::make_unique<ast::Call>(
      frontend::SourceRange{}, ParseAs<ast::Expression>(std::move(fn)),
      MakeFnArgs(std::move(pos_args), std::move(named_args)));
  ast::OverloadSet os;
  os.insert(call_expr->callee());
  auto* expr = call_expr.get();
  module->AppendNode(std::move(call_expr));
  module->compiler.CompleteDeferredBodies();
  return std::pair(std::move(os), expr);
}

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
