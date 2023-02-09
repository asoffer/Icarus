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
#include "module/module.h"
#include "module/resources.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "toolchain/bazel.h"
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

bool Compile(serialization::UniqueModuleId module_id,
             std::string const &source_file, std::string const &module_map_file,
             std::ofstream &output) {
  frontend::SourceIndexer source_indexer;
  auto diagnostic_consumer =
      toolchain::DiagnosticConsumerFromFlag(FLAGS_diagnostics, source_indexer);
  if (not diagnostic_consumer.ok()) {
    std::cerr << diagnostic_consumer.status().message();
    return false;
  }

  std::optional content = base::ReadFileToString(source_file);
  if (not content) {
    // TODO Log an error.
    std::cerr << "no content.\n";
    return false;
  }

  std::string_view file_content =
      source_indexer.insert(serialization::ModuleIndex(0), *std::move(content));

  auto parsed_nodes = frontend::Parse(file_content, **diagnostic_consumer);
  if ((*diagnostic_consumer)->num_consumed() != 0) { return false; }

  ast::Module ast_module;
  ast_module.insert(parsed_nodes.begin(), parsed_nodes.end());

  auto specification = BazelModuleMap(module_map_file);
  if (not specification) {
    // TODO log an error
    std::cerr << "invalid spec.\n";
    return false;
  }
  auto name_resolver = BazelNameResolver(std::move(specification->names));
  ASSERT(name_resolver != nullptr);
  auto &diagnostic_consumer_ref = **diagnostic_consumer;
  module::Resources resources(std::move(module_id), std::move(name_resolver),
                              std::move(*diagnostic_consumer));
  for (auto const &[id, path] : specification->paths) {
    std::ifstream stream(path);
    if (not stream.is_open()) {
      std::cerr << "failed to open " << id.value() << " (" << path << ").\n";
      return false;
    }

    serialization::Module proto;
    if (not proto.ParseFromIstream(&stream)) {
      std::cerr << "failed to parse " << id.value() << " (" << path << ").\n";
      return false;
    }

    if (not resources.LoadFrom(std::move(proto))) {
      // TODO Log an error.
      std::cerr << "failed to load module " << id.value() << " (" << path
                << ").\n";
      return false;
    }
  }

  semantic_analysis::Context context;

  semantic_analysis::TypeVerifier tv(resources, context);
  tv.schedule(&ast_module);
  tv.complete();

  if (diagnostic_consumer_ref.num_consumed() != 0) { return false; }

  semantic_analysis::EmitByteCodeForModule(ast_module, context,
                                           resources.primary_module());

  return resources.primary_module().Serialize(output);
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
  bool success = toolchain::Compile(
      serialization::UniqueModuleId(module_id), absl::GetFlag(FLAGS_source),
      absl::GetFlag(FLAGS_module_map_file), output_stream);
  return success ? 0 : 1;
}
