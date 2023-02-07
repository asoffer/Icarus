#include <cstdlib>
#include <fstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/strings/str_format.h"
#include "base/file.h"
#include "base/log.h"
#include "frontend/parse.h"
#include "module/bazel_module_map.h"
#include "module/module.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "toolchain/flags.h"

ABSL_FLAG(std::vector<std::string>, log, {},
          "Comma-separated list of log keys");
ABSL_FLAG(std::string, diagnostics, "console",
          "Indicates how diagnostics should be emitted. Options: console "
          "(default), or json.");
ABSL_FLAG(std::string, module_identifier, "",
          "Identifier to be used to uniquely identify this module amongst all "
          "modules being linked together, and must not begin with a tilde (~) "
          "character.");
ABSL_FLAG(std::string, source, "",
          "The path to the source file to be compiled.");
ABSL_FLAG(std::string, output, "",
          "The location at which to write the output .icm file.");
ABSL_FLAG(std::string, module_map_file, "",
          "The path to the .icmodmap file describing the module map.");

namespace toolchain {

bool HelpFilter(std::string_view module) { return true; }

void ValidateModuleIdentifier(std::string_view module_identifier) {
  if (module_identifier.empty()) {
    std::cerr << "--module_identifier must not be empty.";
    std::exit(1);
  } else if (module_identifier[0] == '~') {
    std::cerr << "--module_identifier starts with the character '~'. "
                 "Identifiers starting with a tilde are reserved.";
    std::exit(1);
  }
}

void ValidateOutputPath(std::string_view output) {
  if (output.empty()) {
    std::cerr << "--output must not be empty.";
    std::exit(1);
  }
}

absl::StatusOr<std::string> LoadFileContent(
    std::string const &file_name, std::span<std::string const> lookup_paths) {
  if (!file_name.starts_with("/")) {
    for (std::string_view base_path : lookup_paths) {
      if (auto maybe_content =
              base::ReadFileToString(absl::StrCat(base_path, "/", file_name))) {
        return *std::move(maybe_content);
      }
    }
  }
  if (auto maybe_content = base::ReadFileToString(file_name)) {
    return *std::move(maybe_content);
  }
  return absl::NotFoundError(
      absl::StrFormat(R"(Failed to open file "%s")", file_name));
}

bool Compile(std::string const &source_file, std::string const &module_map_file,
             std::ofstream &output) {
  frontend::SourceIndexer source_indexer;
  auto diagnostic_consumer =
      toolchain::DiagnosticConsumerFromFlag(FLAGS_diagnostics, source_indexer);
  if (not diagnostic_consumer.ok()) {
    std::cerr << diagnostic_consumer.status().message();
    return false;
  }

  absl::StatusOr<std::string> content = LoadFileContent(source_file, {});
  if (not content.ok()) {
    // TODO Log an error.
    return false;
  }

  std::string_view file_content =
      source_indexer.insert(module::ModuleIndex(0), *std::move(content));

  auto parsed_nodes = frontend::Parse(file_content, **diagnostic_consumer);
  if ((*diagnostic_consumer)->num_consumed() != 0) { return false; }

  ast::Module ast_module;
  ast_module.insert(parsed_nodes.begin(), parsed_nodes.end());

  std::unique_ptr<module::ModuleMap> module_map =
      module::BazelModuleMap(module_map_file);
  ASSERT(module_map != nullptr);
  semantic_analysis::Context context;

  semantic_analysis::TypeVerifier tv(*module_map, context,
                                     **diagnostic_consumer);
  tv.schedule(&ast_module);
  tv.complete();

  if ((*diagnostic_consumer)->num_consumed() != 0) { return false; }

  module::Module &module = module_map->primary();
  semantic_analysis::EmitByteCodeForModule(ast_module, context, module);

  return module.Serialize(output);
}

}  // namespace toolchain

int main(int argc, char *argv[]) {
  toolchain::InitializeFlags("Icarus compiler", toolchain::HelpFilter);
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);

  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::string module_id = absl::GetFlag(FLAGS_module_identifier);
  toolchain::ValidateModuleIdentifier(module_id);

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (std::string_view key : log_keys) { base::EnableLogging(key); }

  std::string output = absl::GetFlag(FLAGS_output);
  toolchain::ValidateOutputPath(output);

  std::ofstream output_stream(output.c_str(), std::ofstream::out);
  bool success =
      toolchain::Compile(absl::GetFlag(FLAGS_source),
                         absl::GetFlag(FLAGS_module_map_file), output_stream);
  return success ? 0 : 1;
}
