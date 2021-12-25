#include "compiler/importer.h"

#include <cstdio>
#include <string>
#include <string_view>
#include <vector>

#include "absl/cleanup/cleanup.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "base/debug.h"
#include "compiler/context.h"
#include "compiler/resources.h"
#include "compiler/work_graph.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "frontend/source/file_name.h"

namespace compiler {
namespace {

bool FileExists(std::string const& path) {
  if (std::FILE* f = std::fopen(path.c_str(), "r")) {
    std::fclose(f);
    return true;
  }
  return false;
}

// Looks up the given module path to retrieve an absolute path to the module.
frontend::CanonicalFileName ResolveModulePath(
    frontend::CanonicalFileName const& module_path,
    std::vector<std::string> const& lookup_paths) {
  auto save_errno = std::exchange(errno, 0);
  absl::Cleanup c = [&] { errno = save_errno; };

  // Respect absolute paths.
  if (absl::StartsWith(module_path.name(), "/")) {
    return module_path;
  }
  // Check for the module relative to the given lookup paths.
  for (std::string_view base_path : lookup_paths) {
    std::string path = absl::StrCat(base_path, "/", module_path.name());
    if (FileExists(path)) {
      return frontend::CanonicalFileName::Make(frontend::FileName(path));
    }
  }
  // Fall back to using the given path as-is, relative to $PWD.
  return module_path;
}

}  // namespace

ir::ModuleId FileImporter::Import(std::string_view module_locator) {
  auto file_name = frontend::CanonicalFileName::Make(
      frontend::FileName(std::string(module_locator)));
  auto [iter, inserted] = modules_.try_emplace(file_name);
  if (not inserted) { return iter->second->id; }

  auto maybe_file_src = frontend::SourceBufferFromFile(
      ResolveModulePath(file_name, module_lookup_paths_));

  if (not maybe_file_src.ok()) {
    modules_.erase(iter);
    diagnostic_consumer_->Consume(frontend::MissingModule{
        .source    = file_name,
        .requestor = "",
        .reason    = std::string(maybe_file_src.status().message()),
    });
    return ir::ModuleId::Invalid();
  }

  iter->second = std::make_unique<ModuleData>(&*maybe_file_src);
  auto& [id, ir_module, context, module] = *iter->second;
  modules_by_id_.emplace(id, &module);

  for (ir::ModuleId embedded_id : implicitly_embedded_modules()) {
    module.scope().embed(&get(embedded_id).scope());
  }

  auto parsed_nodes = frontend::Parse(*maybe_file_src, *diagnostic_consumer_);
  auto nodes = module.insert(parsed_nodes.begin(), parsed_nodes.end());

  PersistentResources resources{
      .work                = work_set_,
      .module              = &module,
      .diagnostic_consumer = diagnostic_consumer_,
      .importer            = this,
  };
  CompileLibrary(context, resources, nodes);
  return id;
}

}  // namespace compiler
