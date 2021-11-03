#ifndef ICARUS_COMPILER_IMPORTER_H
#define ICARUS_COMPILER_IMPORTER_H

#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "compiler/context.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/source/file_name.h"
#include "ir/value/module_id.h"
#include "module/importer.h"
#include "module/module.h"

namespace compiler {

struct FileImporter : module::Importer {
  explicit FileImporter(std::vector<std::string> module_lookup_paths)
      : module_lookup_paths_(std::move(module_lookup_paths)) {}
  ~FileImporter() override {}

  ir::ModuleId Import(std::string_view module_locator) override;

  module::BasicModule const& get(ir::ModuleId id) override {
    return *modules_by_id_.at(id);
  }

 private:
  struct ModuleData {
    ModuleData()
        : id(ir::ModuleId::New()),
          root_context(&ir_module),
          module(&root_context) {}
    ir::ModuleId id;
    ir::Module ir_module;
    Context root_context;
    CompiledModule module;
  };

  absl::flat_hash_map<frontend::CanonicalFileName, std::unique_ptr<ModuleData>>
      modules_;
  absl::flat_hash_map<ir::ModuleId, CompiledModule*> modules_by_id_;
  std::vector<std::string> module_lookup_paths_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_IMPORTER_H
