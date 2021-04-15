#ifndef ICARUS_MODULE_IMPORTER_H
#define ICARUS_MODULE_IMPORTER_H

#include <string>
#include <string_view>
#include <thread>
#include <vector>

#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "frontend/source/file_name.h"
#include "frontend/source/shared.h"
#include "ir/value/module_id.h"
#include "module/module.h"

// TODO: We should be able to have a diagnostic conusmer without specifying
// source so that file importer can be agnostic to the diagnostic consuming
// mechanism.
namespace module {

// `Importer` is responsible for scheduling any imports requested from an
// `import` expression.
struct Importer {
  virtual ir::ModuleId Import(std::string_view module_locator) = 0;
  virtual BasicModule const& get(ir::ModuleId id) = 0;
};

// Looks up the given module path to retrieve an absolute path to the module.
frontend::CanonicalFileName ResolveModulePath(
    frontend::CanonicalFileName const& module_path,
    std::vector<std::string> const& lookup_paths);

template <typename ModuleType>
struct FileImporter : Importer {
  ir::ModuleId Import(std::string_view module_locator) override {
    auto file_name = frontend::CanonicalFileName::Make(
        frontend::FileName(std::string(module_locator)));
    auto [iter, inserted] = modules_.try_emplace(file_name);
    auto& [id, mod]       = iter->second;
    if (not inserted) { return id; }

    auto maybe_file_src = frontend::FileSource::Make(
        ResolveModulePath(file_name, module_lookup_paths));

    if (not maybe_file_src.ok()) {
      modules_.erase(iter);
      diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
      diag.Consume(frontend::MissingModule{
          .source    = file_name,
          .requestor = "",
          .reason    = stringify(maybe_file_src),
      });
      return ir::ModuleId::Invalid();
    }

    id  = ir::ModuleId::New();
    mod = std::make_unique<ModuleType>();
    modules_by_id_.emplace(id, mod.get());

    std::thread t([this, mod = mod.get(),
                   file_src = std::move(*maybe_file_src)]() mutable {
      mod->template set_diagnostic_consumer<diagnostic::StreamingConsumer>(
          stderr, &file_src);
      mod->AppendNodes(frontend::Parse(file_src, mod->diagnostic_consumer()),
                       mod->diagnostic_consumer(), *this);
    });
    t.detach();
    return id;
  }

  BasicModule const& get(ir::ModuleId id) override {
    return *modules_by_id_.at(id);
  }

  std::vector<std::string> module_lookup_paths;

 private:
  absl::flat_hash_map<frontend::CanonicalFileName,
                      std::pair<ir::ModuleId, std::unique_ptr<ModuleType>>>
      modules_;
  absl::flat_hash_map<ir::ModuleId, ModuleType*> modules_by_id_;
};

}  // namespace module

#endif  // ICARUS_MODULE_IMPORTER_H
