#ifndef ICARUS_TEST_MODULE_H
#define ICARUS_TEST_MODULE_H

#include <string>
#include <utility>
#include <vector>

#include "ast/node.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "compiler/resources.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/aborting.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "frontend/source/string.h"
#include "module/mock_importer.h"
#include "module/module.h"

namespace test {

struct TestModule : compiler::CompiledModule {
  TestModule()
      : compiler::CompiledModule(&context_),
        source_("\n"),
        context_(&ir_module_),
        work_graph_(compiler::PersistentResources{
            .work                = &work_set,
            .module              = this,
            .diagnostic_consumer = &consumer,
            .importer            = &importer,
        }) {}

  void AppendCode(std::string code) {
    code.push_back('\n');
    source_.buffer().AppendChunk(std::move(code));
    diagnostic::AbortingConsumer diag(&source_);
    auto stmts = frontend::Parse(source_.buffer(), diag,
                                 source_.buffer().num_chunks() - 1);
    auto nodes = insert(stmts.begin(), stmts.end());
    compiler::Compiler c(&context(), resources());
    c.set_work_resources(work_graph_.work_resources());
    for (auto const* node : nodes) {
      auto const* decl = node->if_as<ast::Declaration>();
      if (decl and (decl->flags() & ast::Declaration::f_IsConst)) {
        c.VerifyType(node);
      }
    }
    for (auto const* node : nodes) {
      auto const* decl = node->if_as<ast::Declaration>();
      if (not decl or not(decl->flags() & ast::Declaration::f_IsConst)) {
        c.VerifyType(node);
      }
    }
    Complete();
  }

  void Complete() { work_graph_.complete(); }

  template <typename NodeType>
  NodeType const* Append(std::string code) {
    code.push_back('\n');
    source_.buffer().AppendChunk(std::move(code));

    diagnostic::AbortingConsumer diag(&source_);
    auto stmts = frontend::Parse(source_.buffer(), diag,
                                 source_.buffer().num_chunks() - 1);
    if (auto* ptr = stmts[0]->template if_as<NodeType>()) {
      std::vector<std::unique_ptr<ast::Node>> ns;
      ns.push_back(std::move(stmts[0]));
      auto nodes = insert(ns.begin(), ns.end());
      compiler::Compiler c(&context(), resources());
      c.set_work_resources(work_graph_.work_resources());
      for (auto const* node : nodes) {
        auto const* decl = node->if_as<ast::Declaration>();
        if (decl and (decl->flags() & ast::Declaration::f_IsConst)) {
          c.VerifyType(node);
        }
      }
      for (auto const* node : nodes) {
        auto const* decl = node->if_as<ast::Declaration>();
        if (not decl or not(decl->flags() & ast::Declaration::f_IsConst)) {
          c.VerifyType(node);
        }
      }
      Complete();
      return ptr;
    } else {
      return nullptr;
    }
  }

  compiler::PersistentResources const& resources() const {
    return work_graph_.resources();
  }

  compiler::WorkResources work_resources() {
    return work_graph_.work_resources();
  }

  void CompileImportedLibrary(compiler::CompiledModule& imported_mod,
                              std::string_view name, std::string content) {
    auto id = ir::ModuleId::New();

    compiler::PersistentResources import_resources{
        .work                = &work_set,
        .module              = &imported_mod,
        .diagnostic_consumer = &consumer,
        .importer            = &importer,
    };

    frontend::SourceBuffer buffer(std::move(content));
    auto parsed_nodes = frontend::Parse(buffer, consumer);
    compiler::CompileLibrary(
        imported_mod.context(), import_resources,
        imported_mod.insert(parsed_nodes.begin(), parsed_nodes.end()));

    ON_CALL(importer, Import(testing::Eq(name)))
        .WillByDefault([id](std::string_view) { return id; });
    ON_CALL(importer, get(id))
        .WillByDefault([&imported_mod](ir::ModuleId) -> module::BasicModule& {
          return imported_mod;
        });
  }

  module::MockImporter importer;
  diagnostic::TrackingConsumer consumer;
  compiler::WorkSet work_set;
 private:
  frontend::StringSource source_;
  ir::Module ir_module_;
  compiler::Context context_;
  compiler::WorkGraph work_graph_;
};

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
