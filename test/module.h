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
#include "frontend/source/buffer.h"
#include "module/mock_importer.h"
#include "module/module.h"

namespace test {

// Used to guarantee that context is initialized before module.
struct ContextHolder {
 protected:
  ContextHolder() : ctx_(&ir_module_) {}
  ir::Module ir_module_;
  compiler::Context ctx_;
};

struct TestModule : ContextHolder, compiler::CompiledModule {
  TestModule()
      : ContextHolder(),
        compiler::CompiledModule(&source_, &ctx_),
        source_("test-module", "\n"),
        work_graph_(compiler::PersistentResources{
            .work                = &work_set,
            .module              = this,
            .diagnostic_consumer = &consumer,
            .importer            = &importer,
        }) {}

  void AppendCode(std::string code) {
    code.push_back('\n');
    source_.AppendChunk(std::move(code));
    size_t num = consumer.num_consumed();
    auto stmts = frontend::Parse(source_, consumer, source_.num_chunks() - 1);
    if (consumer.num_consumed() != num) { return; }
    auto nodes = insert(stmts.begin(), stmts.end());
    compiler::CompilationData data{
        .context        = &context(),
        .work_resources = work_graph_.work_resources(),
        .resources      = resources(),
    };
    compiler::Compiler c(&data);
    for (auto const* node : nodes) {
      auto const* decl = node->if_as<ast::Declaration>();
      if (decl and (decl->flags() & ast::Declaration::f_IsConst)) {
        VerifyType(c, node);
      }
    }
    for (auto const* node : nodes) {
      auto const* decl = node->if_as<ast::Declaration>();
      if (not decl or not(decl->flags() & ast::Declaration::f_IsConst)) {
        VerifyType(c, node);
      }
    }
    Complete();
  }

  void Complete() { work_graph_.complete(); }

  template <typename NodeType>
  NodeType const* Append(std::string code) {
    code.push_back('\n');
    source_.AppendChunk(std::move(code));

    size_t num = consumer.num_consumed();
    auto stmts = frontend::Parse(source_, consumer, source_.num_chunks() - 1);
    if (consumer.num_consumed() != num) { return nullptr; }
    if (auto* ptr = stmts[0]->template if_as<NodeType>()) {
      std::vector<std::unique_ptr<ast::Node>> ns;
      ns.push_back(std::move(stmts[0]));
      auto nodes = insert(ns.begin(), ns.end());
      compiler::CompilationData data{
          .context        = &context(),
          .work_resources = work_graph_.work_resources(),
          .resources      = resources(),
      };
      compiler::Compiler c(&data);
      for (auto const* node : nodes) {
        auto const* decl = node->if_as<ast::Declaration>();
        if (decl and (decl->flags() & ast::Declaration::f_IsConst)) {
          VerifyType(c, node);
        }
      }
      for (auto const* node : nodes) {
        auto const* decl = node->if_as<ast::Declaration>();
        if (not decl or not(decl->flags() & ast::Declaration::f_IsConst)) {
          VerifyType(c, node);
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

    auto& buffer      = imported_.emplace_back(std::move(content));
    size_t num        = consumer.num_consumed();
    auto parsed_nodes = frontend::Parse(buffer, consumer);
    if (consumer.num_consumed() != num) { return; }
    compiler::CompileModule(
        imported_mod.context(), import_resources,
        imported_mod.insert(parsed_nodes.begin(), parsed_nodes.end()));

    ON_CALL(importer, Import(testing::_, testing::Eq(name)))
        .WillByDefault(
            [id](module::Module const*, std::string_view) { return id; });
    ON_CALL(importer, get(id))
        .WillByDefault([&imported_mod](ir::ModuleId) -> module::Module& {
          return imported_mod;
        });
  }

  module::MockImporter importer;
  diagnostic::TrackingConsumer consumer;
  compiler::WorkSet work_set;

 private:
  frontend::SourceBuffer source_;
  std::deque<frontend::SourceBuffer> imported_;
  compiler::WorkGraph work_graph_;
};

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
