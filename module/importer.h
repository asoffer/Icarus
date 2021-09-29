#ifndef ICARUS_MODULE_IMPORTER_H
#define ICARUS_MODULE_IMPORTER_H

#include <string>
#include <string_view>
#include <thread>
#include <vector>

#include "base/any_invocable.h"
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
  virtual BasicModule const& get(ir::ModuleId id)              = 0;
  virtual void CompleteWork()                                  = 0;

  bool SetImplicitlyEmbeddedModules(
      absl::Span<std::string const> module_locators);

  absl::Span<ir::ModuleId const> implicitly_embedded_modules() const {
    return embedded_module_ids_;
  }

 private:
  std::vector<ir::ModuleId> embedded_module_ids_;
};

// Looks up the given module path to retrieve an absolute path to the module.
frontend::CanonicalFileName ResolveModulePath(
    frontend::CanonicalFileName const& module_path,
    std::vector<std::string> const& lookup_paths);

template <typename ModuleType>
struct FileImporter : Importer {
  explicit FileImporter(
      base::any_invocable<void(ModuleType*, base::PtrSpan<ast::Node const>)>
          compile)
      : compile_(std::move(compile)) {}

  ~FileImporter() { CompleteWork(); }

  ir::ModuleId Import(std::string_view module_locator) override {
    auto file_name = frontend::CanonicalFileName::Make(
        frontend::FileName(std::string(module_locator)));
    auto [iter, inserted] = modules_.try_emplace(file_name);
    auto& [id, mod]       = iter->second;
    if (not inserted) { return id; }

    auto maybe_file_src = frontend::FileSource::Make(
        ResolveModulePath(file_name, module_lookup_paths));

    diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
    if (not maybe_file_src.ok()) {
      modules_.erase(iter);
      diag.Consume(frontend::MissingModule{
          .source    = file_name,
          .requestor = "",
          .reason    = stringify(maybe_file_src),
      });
      return ir::ModuleId::Invalid();
    }

    id  = ir::ModuleId::New();
    mod                 = std::make_unique<ModuleType>();
    ModuleType* mod_ptr = mod.get();
    ir::ModuleId id_copy = id;
    modules_by_id_.emplace(id_copy, mod_ptr);

    for (ir::ModuleId embedded_id : implicitly_embedded_modules()) {
      mod_ptr->embed(get(embedded_id));
    }

    auto nodes =
        mod_ptr->InitializeNodes(frontend::Parse(maybe_file_src->buffer(), diag));

    // work_.emplace_back(
    //    [this, mod = mod.get(), nodes,
    //     file_src = std::move(*maybe_file_src)]() mutable {
    mod_ptr->template set_diagnostic_consumer<diagnostic::StreamingConsumer>(
        stderr, &*maybe_file_src);
    compile_(mod_ptr, nodes);
    //    });
    // TODO: We're running this serially.  We're okay with this because the plan
    // is to separate these into separate tool invocations.
    return id_copy;
  }

  void CompleteWork() override {
    while (not work_.empty()) {
      auto work = std::exchange(work_, {});
      for (auto& t : work) { t.join(); }
    }
  }

  BasicModule const& get(ir::ModuleId id) override {
    return *modules_by_id_.at(id);
  }

  std::vector<std::string> module_lookup_paths;

 private:
  std::vector<std::thread> work_;
  absl::flat_hash_map<frontend::CanonicalFileName,
                      std::pair<ir::ModuleId, std::unique_ptr<ModuleType>>>
      modules_;
  absl::flat_hash_map<ir::ModuleId, ModuleType*> modules_by_id_;
  // TODO: Our any_invocable implementation doesn't handle references yet.
  base::any_invocable<void(ModuleType*, base::PtrSpan<ast::Node const>)>
      compile_;
};

}  // namespace module

#endif  // ICARUS_MODULE_IMPORTER_H
