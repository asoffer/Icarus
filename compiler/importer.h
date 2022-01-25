#ifndef ICARUS_COMPILER_IMPORTER_H
#define ICARUS_COMPILER_IMPORTER_H

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "compiler/context.h"
#include "compiler/module.h"
#include "compiler/work_item.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/source/file_name.h"
#include "ir/value/module_id.h"
#include "module/importer.h"
#include "module/module.h"

namespace compiler {

struct FileImporter : module::Importer {
  explicit FileImporter(WorkSet* work_set,
                        diagnostic::DiagnosticConsumer* diagnostic_consumer,
                        std::vector<std::string> module_lookup_paths)
      : work_set_(ASSERT_NOT_NULL(work_set)),
        diagnostic_consumer_(ASSERT_NOT_NULL(diagnostic_consumer)),
        module_lookup_paths_(std::move(module_lookup_paths)) {}
  ~FileImporter() override {}

  ir::ModuleId Import(module::BasicModule const* requestor,
                      std::string_view module_locator) override;

  module::BasicModule& get(ir::ModuleId id) override {
    return *modules_by_id_.at(id);
  }

  void set_subroutine(module::BasicModule const* mod,
                      ir::Subroutine subroutine) {
    subroutine_by_module_.emplace(mod, std::move(subroutine));
  }

  void ForEachSubroutine(std::invocable<ir::Subroutine const&> auto&& f) const {
    graph_.topologically([&](module::BasicModule const* mod) {
      auto iter = subroutine_by_module_.find(mod);
      ASSERT(iter != subroutine_by_module_.end());
      f(iter->second);
    });
  }

 private:
  struct ModuleData {
    // TODO: SourceBuffer*
    ModuleData(frontend::SourceBuffer* buffer)
        : id(ir::ModuleId::New()),
          root_context(&ir_module),
          module(buffer, &root_context) {}
    ir::ModuleId id;
    ir::Module ir_module;
    Context root_context;
    CompiledModule module;
  };

  WorkSet* work_set_;
  absl::flat_hash_map<frontend::CanonicalFileName, std::unique_ptr<ModuleData>>
      modules_;
  absl::flat_hash_map<ir::ModuleId, CompiledModule*> modules_by_id_;
  base::Graph<module::BasicModule const*> graph_;
  absl::flat_hash_map<module::BasicModule const*, ir::Subroutine>
      subroutine_by_module_;
  diagnostic::DiagnosticConsumer* diagnostic_consumer_;
  std::vector<std::string> module_lookup_paths_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_IMPORTER_H
