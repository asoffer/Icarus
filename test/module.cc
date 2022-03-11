#include "test/module.h"

#include "compiler/builtin_module.h"
#include "module/trivial_importer.h"

namespace test {
namespace {

void ProcessModule(compiler::Context& context,
                   compiler::PersistentResources const& resources,
                   base::PtrSpan<ast::Node const> nodes) {
  compiler::WorkGraph w(resources);
  (compiler::VerifyNodesSatisfying(compiler::IsConstantDeclaration, context, w,
                                   nodes, true) and
   compiler::VerifyNodesSatisfying(compiler::IsNotConstantDeclaration, context,
                                   w, nodes));
  w.complete();
}

}  // namespace
CompilerInfrastructure::CompilerInfrastructure()
    : shared_context_(compiler::MakeBuiltinModule()), context_(&ir_module_) {}

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

TestModule& CompilerInfrastructure::add_module(std::string name, std::string code) {
  auto [id, mod] = shared_context_.module_table().add_module<TestModule>(
      std::move(name), &context_);
  mod->set_id(id);

  code.push_back('\n');

  std::string_view content = source_indexer_.insert(id, std::move(code));

  size_t num = consumer_.num_consumed();
  auto stmts = frontend::Parse(content, consumer_);
  if (consumer_.num_consumed() != num) { return *mod; }
  auto nodes = mod->insert(stmts.begin(), stmts.end());

  ProcessModule(context_,
                {.work                = &work_set_,
                 .module              = mod,
                 .diagnostic_consumer = &consumer_,
                 .importer            = &importer_,
                 .shared_context      = &shared_context_},
                nodes);

  return *mod;
}

}  // namespace test
