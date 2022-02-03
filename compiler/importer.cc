#include "compiler/importer.h"

#include <cerrno>
#include <cstdio>
#include <string>
#include <string_view>
#include <vector>
#include <version>

#include "absl/cleanup/cleanup.h"
#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "base/debug.h"
#include "compiler/context.h"
#include "compiler/resources.h"
#include "compiler/work_graph.h"
#include "frontend/parse.h"

namespace compiler {

absl::StatusOr<std::string> LoadFileContent(
    std::string const& file_name, absl::Span<std::string const> lookup_paths) {
  auto load_file =
      [](std::string const& file_name) -> std::optional<std::string> {
    std::optional<std::string> result = std::nullopt;
    auto save_errno                   = std::exchange(errno, 0);
    std::FILE* file                   = std::fopen(file_name.c_str(), "r");
    absl::Cleanup errno_replacer      = [&] { errno = save_errno; };

    if (not file) { return std::nullopt; }
    absl::Cleanup closer = [&] { std::fclose(file); };

    std::fseek(file, 0, SEEK_END);
    size_t file_size = std::ftell(file);
    std::rewind(file);

    result.emplace();
#if defined(__cpp_lib_string_resize_and_overwrite)
    result->resize_and_overwrite(file_size, [&](char* buffer, size_t size) {
      std::fread(buffer, sizeof(char), file_size, file);
    });
#else
    result->resize(file_size, '\0');
    std::fread(result->data(), sizeof(char), file_size, file);
#endif
    return result;
  };

  if (!file_name.starts_with("/")) {
    for (std::string_view base_path : lookup_paths) {
      if (auto maybe_content =
              load_file(absl::StrCat(base_path, "/", file_name))) {
        return *std::move(maybe_content);
      }
    }
  }
  if (auto maybe_content = load_file(file_name)) {
    return *std::move(maybe_content);
  }
  return absl::NotFoundError(
      absl::StrFormat(R"(Failed to open file "%s")", file_name));
}

ir::ModuleId FileImporter::Import(module::Module const* requestor,
                                  std::string_view module_locator) {
  // TODO: Canonicalize file names.
  std::string file_name = std::string(module_locator);
  auto [iter, inserted] = modules_.try_emplace(file_name);
  if (not inserted) {
    // Even if it's already been imported, this edge may not have been added
    // yet.
    graph_.add_edge(requestor, &get(iter->second->id));
    return iter->second->id;
  }

  absl::StatusOr<std::string> file_content =
      LoadFileContent(file_name, module_lookup_paths_);
  if (not file_content.ok()) {
    modules_.erase(iter);
    diagnostic_consumer_->Consume(MissingModule{
        .source    = file_name,
        .requestor = "",
        .reason    = std::string(file_content.status().message()),
    });
    return ir::ModuleId::Invalid();
  }
  ir::ModuleId id = ir::ModuleId::New();
  std::string_view content =
      source_indexer_.insert(id, *std::move(file_content));

  iter->second = std::make_unique<ModuleData>(id, content);
  auto& [mod_id, ir_module, context, module] = *iter->second;
  modules_by_id_.emplace(id, &module);

  for (ir::ModuleId embedded_id : implicitly_embedded_modules()) {
    module.scope().embed(&get(embedded_id));
  }

  auto parsed_nodes = frontend::Parse(content, *diagnostic_consumer_);
  auto nodes        = module.insert(parsed_nodes.begin(), parsed_nodes.end());

  PersistentResources resources{
      .work                = work_set_,
      .module              = &module,
      .diagnostic_consumer = diagnostic_consumer_,
      .importer            = this,
  };

  graph_.add_edge(requestor, &module);
  std::optional subroutine = CompileModule(context, resources, nodes);
  if (subroutine) {
    subroutine_by_module_.emplace(&module, *std::move(subroutine));
  }
  // A nullopt subroutine means there were errors. We can still emit the `id`.
  // Errors will already be diagnosed.
  return id;
}

}  // namespace compiler
