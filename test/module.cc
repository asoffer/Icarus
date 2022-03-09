#include "test/module.h"

#include "compiler/builtin_module.h"
#include "module/trivial_importer.h"

namespace test {

CompilerInfrastructure::CompilerInfrastructure() : context_(&ir_module_) {
  shared_context_.module_table().add_module(compiler::MakeBuiltinModule());
}

std::optional<ir::CompleteResultBuffer> CompilerInfrastructure::Evaluate(
    compiler::CompiledModule& module, ast::Expression const* e) {
  compiler::WorkSet work_set;
  compiler::PersistentResources resources{
      .work                = &work_set,
      .module              = &module,
      .diagnostic_consumer = &consumer_,
      .importer            = &importer_,
      .shared_context      = &shared_context_,
  };

  compiler::WorkGraph work_graph(resources);

  compiler::CompilationData data{.context        = &module.context(),
                                 .work_resources = work_graph.work_resources(),
                                 .resources      = resources};
  compiler::Compiler c(&data);

  auto qts = module.context().qual_types(e);
  if (qts.size() != 1) {
    ADD_FAILURE() << "Expected evaluation of a single expression but got "
                  << qts.size();
    return std::nullopt;
  }
  auto t = qts[0].type();
  if (not t.valid()) {
    ADD_FAILURE() << "Type is unexpectedly invalid.";
    return std::nullopt;
  }
  return c.EvaluateToBufferOrDiagnose(
      type::Typed<ast::Expression const *>(e, t));
}

TestModule& CompilerInfrastructure::add_module(std::string code) {
  return add_module(
      absl::StrCat("~test-module-", shared_context_.module_table().size()),
      std::move(code));
}

TestModule& CompilerInfrastructure::add_module(std::string name, std::string code) {
  auto [id, mod] = shared_context_.module_table().add_module<TestModule>(
      std::move(name), &context_);
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

  compiler::CompileModule(context_,
                          {.work                = &work_set_,
                           .module              = mod,
                           .diagnostic_consumer = &consumer_,
                           .importer            = &importer_,
                           .shared_context      = &shared_context_},
                          nodes);

  return *mod;
}

void TestModule::CompileImportedLibrary(TestModule& imported_mod,
                                        std::string_view name, std::string s) {
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

}  // namespace test
