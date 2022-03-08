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
#include "test/evaluation.h"

namespace test {

struct TestModule;

struct CompilerInfrastructure {
  CompilerInfrastructure() : context_(&ir_module_) {}

  TestModule& add_module(std::string code);

 private:
  module::SharedContext shared_context_;
  ir::Module ir_module_;
  compiler::Context context_;
  module::MockImporter importer_;
  diagnostic::TrackingConsumer consumer_;
  compiler::WorkSet work_set_;
  frontend::SourceIndexer source_indexer_;
};

struct TestModule : compiler::CompiledModule {
  explicit TestModule(std::string identifier, compiler::Context* context)
      : compiler::CompiledModule(std::move(identifier), context) {}

  void set_id(ir::ModuleId id) { id_ = id; }

  template <std::derived_from<ast::Node> NodeType>
  NodeType const* get() {
    return &module().stmts().back()->as<NodeType>();
  }

  void CompileImportedLibrary(TestModule& imported_mod, std::string_view name,
                              std::string s) {
    NOT_YET();
    // compiler::PersistentResources import_resources{
    //     .work                = &work_set,
    //     .module              = this,
    //     .diagnostic_consumer = &consumer,
    //     .importer            = &importer,
    //     .shared_context      = &internal_module::shared_context,
    // };

    // std::string_view content = indexer_.insert(imported_mod.id_, std::move(s));
    // size_t num        = consumer.num_consumed();
    // auto parsed_nodes = frontend::Parse(content, consumer);
    // if (consumer.num_consumed() != num) { return; }
    // compiler::CompileModule(
    //     imported_mod.context(), import_resources,
    //     imported_mod.insert(parsed_nodes.begin(), parsed_nodes.end()));

    // ON_CALL(importer, Import(testing::_, testing::Eq(name)))
    //     .WillByDefault([imported_mod.id_](module::Module const*,
    //                                       std::string_view) { return id; });
    // ON_CALL(importer, get(imported_mod.id_))
    //     .WillByDefault([&imported_mod](ir::ModuleId) -> module::Module& {
    //       return *imported_mod;
    //     });
  }

 private:
  ir::ModuleId id_;
};

TestModule& CompilerInfrastructure::add_module(std::string code) {
  auto [id, mod] = shared_context_.module_table().add_module<TestModule>(
      absl::StrCat("~test-module-", shared_context_.module_table().size()),
      &context_);
  mod->set_id(id);

  code.push_back('\n');

  std::string_view content = source_indexer_.insert(id, std::move(code));

  size_t num = consumer_.num_consumed();
  auto stmts = frontend::Parse(content, consumer_);
  if (consumer_.num_consumed() != num) {
    ADD_FAILURE() << "Parsing failure.";
    return *mod;
  }
  auto nodes = mod->insert(stmts.begin(), stmts.end());

  compiler::WorkGraph work_graph(compiler::PersistentResources{
      .work                = &work_set_,
      .module              = mod,
      .diagnostic_consumer = &consumer_,
      .importer            = &importer_,
      .shared_context      = &shared_context_,
  });
  compiler::CompilationData data{
      .context        = &context_,
      .work_resources = work_graph.work_resources(),
      .resources      = work_graph.resources(),
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
  work_graph.complete();

  return *mod;
}

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H
