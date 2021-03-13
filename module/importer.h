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

// TODO: We should be able to have a diagnostic conusmer without specifying
// source so that file importer can be agnostic to the diagnostic consuming
// mechanism.
namespace module {

// `Importer` is responsible for scheduling any imports requested from an
// `import` expression.
struct Importer {
  virtual ir::ModuleId Import(std::string_view module_locator) = 0;
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
    auto [id, mod, inserted] = ir::ModuleId::FromFile<ModuleType>(file_name);
    if (not inserted) { return id; }

    frontend::CanonicalFileName const& module_path = id.filename();
    if (auto maybe_file_src = frontend::FileSource::Make(
            ResolveModulePath(module_path, module_lookup_paths));
        maybe_file_src.ok()) {
      std::thread t(
          [this, mod = mod, file_src = std::move(*maybe_file_src)]() mutable {
            diagnostic::StreamingConsumer diag(stderr, &file_src);
            mod->AppendNodes(frontend::Parse(file_src, diag), diag, *this);
          });
      t.detach();
      return id;
    } else {
      diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());
      diag.Consume(frontend::MissingModule{
          .source    = module_path,
          .requestor = "",
          .reason    = stringify(maybe_file_src),
      });
      return ir::ModuleId::Invalid();
    }
  }

  std::vector<std::string> module_lookup_paths;
};

}  // namespace module

#endif  // ICARUS_MODULE_IMPORTER_H
