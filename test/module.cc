#include "test/module.h"

#include "compiler/builtin_module.h"
#include "frontend/lex/lex.h"
#include "frontend/parse.h"
#include "module/trivial_importer.h"

namespace test {
namespace {

struct TestImporter : module::Importer {
  TestImporter(module::ModuleTable* table) : table_(*ASSERT_NOT_NULL(table)) {}
  virtual ~TestImporter() {}

  ir::ModuleId Import(module::Module const*,
                      std::string_view module_locator) override {
    return table_.module(module_locator).first;
  }
  module::Module& get(ir::ModuleId id) override {
    return *ASSERT_NOT_NULL(table_.module(id));
  }

 private:
  module::ModuleTable& table_;
};

}  // namespace

CompilerInfrastructure::CompilerInfrastructure()
    : shared_context_(compiler::MakeBuiltinModule()),
      importer_(
          std::make_unique<TestImporter>(&shared_context_.module_table())) {}

CompilerInfrastructure::CompilerInfrastructure(
    std::unique_ptr<module::Importer> i)
    : shared_context_(compiler::MakeBuiltinModule()), importer_(std::move(i)) {}

std::optional<ir::CompleteResultBuffer> CompilerInfrastructure::Evaluate(
    compiler::CompiledModule& module, ast::Expression const* e) {
  compiler::WorkSet work_set;
  compiler::PersistentResources resources{
      .work                = &work_set,
      .module              = &module,
      .diagnostic_consumer = &consumer_,
      .importer            = importer_.get(),
      .shared_context      = &shared_context_,
  };

  compiler::WorkGraph work_graph(resources);
  absl::Cleanup cleanup = [&] { work_graph.complete(); };

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

  auto result = work_graph.work_resources().evaluate(
      module.context(), type::Typed<ast::Expression const*>(e, t));
  if (auto* diagnostics =
          std::get_if<std::vector<diagnostic::ConsumedMessage>>(&result)) {
    ADD_FAILURE() << "Unable to evaluate.";
    return std::nullopt;
  } else {
    return std::get<ir::CompleteResultBuffer>(std::move(result));
  }
}

TestModule& CompilerInfrastructure::add_module(std::string code) {
  return add_module(
      absl::StrCat("~test-module-", shared_context_.module_table().size()),
      std::move(code));
}

TestModule& CompilerInfrastructure::add_module(std::string name,
                                               std::string code) {
  auto [id, mod] =
      shared_context_.module_table().add_module<TestModule>(std::move(name));
  mod->set_id(id);

  code.push_back('\n');

  std::string_view content = source_indexer_.insert(id, std::move(code));

  size_t num = consumer_.num_consumed();
  ASSIGN_OR(return *mod,  //
                   auto result, frontend::Lex(content, consumer_));
  ASSIGN_OR(return *mod,  //
                   auto m, frontend::ParseModule(result.lexemes_, consumer_));
  m.Initialize();
  auto nodes = mod->set_module(std::move(m));

  compiler::CompileModule(mod->context(),
                          {.work                = &work_set_,
                           .module              = mod,
                           .diagnostic_consumer = &consumer_,
                           .importer            = importer_.get(),
                           .shared_context      = &shared_context_},
                          nodes);

  return *mod;
}

}  // namespace test
