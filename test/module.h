#ifndef ICARUS_TEST_MODULE_H
#define ICARUS_TEST_MODULE_H

#include <string>
#include <utility>
#include <vector>

#include "ast/node.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "compiler/resources.h"
#include "compiler/verify/verify.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "frontend/source_indexer.h"
#include "module/mock_importer.h"
#include "module/module.h"

namespace test {

struct TestModule;

struct CompilerInfrastructure {
  CompilerInfrastructure();

  auto diagnostics() const { return consumer_.diagnostics(); }
  auto& importer() { return importer_; }

  TestModule& add_module(std::string name, std::string code);
  TestModule& add_module(std::string code);

  // Evaluates the expression `e` which must be part of the given `module`.
  // Returns a buffer holding its value if evaluation succeeds and nullopt
  // (alerting GoogleTest to failures) otherwise.
  std::optional<ir::CompleteResultBuffer> Evaluate(
      compiler::CompiledModule& module, ast::Expression const* e);

 private:
  frontend::SourceIndexer source_indexer_;
  module::SharedContext shared_context_;
  ir::Module ir_module_;
  compiler::Context context_;
  module::MockImporter importer_;
  diagnostic::TrackingConsumer consumer_;
  compiler::WorkSet work_set_;
};

struct TestModule : compiler::CompiledModule {
  explicit TestModule(std::string identifier, compiler::Context* context)
      : compiler::CompiledModule(std::move(identifier), context) {}

  void set_id(ir::ModuleId id) { id_ = id; }

  template <std::derived_from<ast::Node> NodeType>
  NodeType const* get() {
    return &module().stmts().back()->as<NodeType>();
  }

 private:
  ir::ModuleId id_;
};

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
