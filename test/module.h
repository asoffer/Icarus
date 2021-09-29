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
#include "compiler/compiler.h"
#include "diagnostic/consumer/aborting.h"
#include "diagnostic/consumer/tracking.h"
#include "module/mock_importer.h"
#include "module/module.h"

namespace test {

struct TestModule : compiler::CompiledModule {
  TestModule()
      : compiler({
            .context             = context(),
            .diagnostic_consumer = consumer,
            .importer            = importer,
        }),
        source("\n") {}

  void AppendCode(std::string code) {
    code.push_back('\n');
    source.buffer().AppendChunk(std::move(code));
    AppendNodes(frontend::Parse(source.buffer(), consumer,
                                source.buffer().num_chunks() - 1),
                consumer, importer);
  }

  template <typename NodeType>
  NodeType const* Append(std::string code) {
    code.push_back('\n');
    source.buffer().AppendChunk(std::move(code));

    diagnostic::AbortingConsumer diag(&source);
    auto stmts = frontend::Parse(source.buffer(), diag,
                                 source.buffer().num_chunks() - 1);
    if (auto* ptr = stmts[0]->template if_as<NodeType>()) {
      std::vector<std::unique_ptr<ast::Node>> nodes;
      nodes.push_back(std::move(stmts[0]));
      AppendNodes(std::move(nodes), consumer, importer);
      return ptr;
    } else {
      return nullptr;
    }
  }

  compiler::WorkGraph work_graph;
  module::MockImporter importer;
  diagnostic::TrackingConsumer consumer;
  compiler::Compiler compiler;

 protected:
  void ProcessNodes(base::PtrSpan<ast::Node const> nodes,
                    diagnostic::DiagnosticConsumer& diag,
                    module::Importer&) override {
    compiler.VerifyAll(nodes);
    work_graph.complete();
  }

 private:
  frontend::StringSource source;
};


struct TestResources {
  diagnostic::TrackingConsumer consumer;
  module::MockImporter importer;
};

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
