#ifndef ICARUS_COMPILER_IMPORTER_H
#define ICARUS_COMPILER_IMPORTER_H

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "compiler/builtin_module.h"
#include "compiler/context.h"
#include "compiler/module.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/source_indexer.h"
#include "ir/value/module_id.h"
#include "module/importer.h"
#include "module/module.h"
#include "module/precompiled.h"

namespace compiler {

struct MissingModuleMap {
  static constexpr std::string_view kCategory = "tool-failure";
  static constexpr std::string_view kName     = "missing-module-map";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Could not find module map \"%s\"", module_map));
  }

  std::string module_map;
};

struct MissingModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-module";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(diagnostic::Text(
        "Could not find module named \"%s\":\n%s", source, reason));
  }

  std::string source;
  std::string requestor;  // TODO: Set this correctly or remove it.
  std::string reason;
};

struct FileImporter : module::Importer {
  explicit FileImporter(
      WorkSet* work_set, diagnostic::DiagnosticConsumer* diagnostic_consumer,
      frontend::SourceIndexer* source_indexer,
      absl::flat_hash_map<std::string, std::string> module_map,
      std::vector<std::string> module_lookup_paths)
      : work_set_(ASSERT_NOT_NULL(work_set)),
        builtin_(MakeBuiltinModule()),
        modules_by_id_{{ir::ModuleId::Builtin(), &builtin_}},
        diagnostic_consumer_(ASSERT_NOT_NULL(diagnostic_consumer)),
        module_map_(std::move(module_map)),
        module_lookup_paths_(std::move(module_lookup_paths)),
        source_indexer_(*ASSERT_NOT_NULL(source_indexer)) {}

  ~FileImporter() override {}

  ir::ModuleId Import(module::Module const* requestor,
                      std::string_view module_locator) override;

  module::Module& get(ir::ModuleId id) override {
    return *modules_by_id_.at(id);
  }

  void set_subroutine(module::Module const* mod, ir::Subroutine subroutine) {
    graph_.add_node(mod);
    subroutine_by_module_.emplace(mod, std::move(subroutine));
  }

  void ForEachSubroutine(std::invocable<ir::Subroutine const&> auto&& f) const {
    graph_.topologically([&](module::Module const* mod) {
      auto iter = subroutine_by_module_.find(mod);
      // TODO: Every module should have a subroutine but we're not yet handling
      // this for precompiled modules.
      if (iter != subroutine_by_module_.end()) { f(iter->second); }
    });
  }

 private:
  struct ModuleData {
    ModuleData(std::string_view file_content)
        : root_context(&ir_module), module(file_content, &root_context) {}
    ir::Module ir_module;
    Context root_context;
    CompiledModule module;
  };

  WorkSet* work_set_;
  absl::flat_hash_map<
      std::string,
      std::pair<ir::ModuleId,
                std::variant<std::unique_ptr<module::PrecompiledModule>,
                             std::unique_ptr<ModuleData>>>>
      modules_;
  module::BuiltinModule builtin_;
  absl::flat_hash_map<ir::ModuleId, module::Module*> modules_by_id_;
  base::Graph<module::Module const*> graph_;
  absl::flat_hash_map<module::Module const*, ir::Subroutine>
      subroutine_by_module_;
  diagnostic::DiagnosticConsumer* diagnostic_consumer_;
  absl::flat_hash_map<std::string, std::string> module_map_;
  std::vector<std::string> module_lookup_paths_;
  frontend::SourceIndexer& source_indexer_;
};

absl::StatusOr<std::string> LoadFileContent(
    std::string const& file_name,
    absl::Span<std::string const> lookup_paths = {});

std::optional<absl::flat_hash_map<std::string, std::string>> MakeModuleMap(
    std::string const& file_name);

}  // namespace compiler

#endif  // ICARUS_COMPILER_IMPORTER_H
