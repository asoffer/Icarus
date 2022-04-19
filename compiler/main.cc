#include <cstdlib>
#include <fstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/match.h"
#include "absl/strings/str_split.h"
#include "base/log.h"
#include "base/untyped_buffer.h"
#include "compiler/flags.h"
#include "compiler/importer.h"
#include "compiler/module.h"
#include "compiler/work_graph.h"
#include "frontend/parse.h"
#include "ir/interpreter/execution_context.h"
#include "ir/subroutine.h"
#include "module/map.h"
#include "module/map_bazel.h"
#include "module/module.h"
#include "module/shared_context.h"
#include "opt/opt.h"

ABSL_FLAG(std::vector<std::string>, log, {},
          "Comma-separated list of log keys");
ABSL_FLAG(std::string, link, "",
          "Library to be dynamically loaded by the compiler to be used "
          "at compile-time. Libraries will not be unloaded.");
ABSL_FLAG(bool, opt_ir, false, "Optimize intermediate representation.");
ABSL_FLAG(std::vector<std::string>, module_paths, {},
          "Comma-separated list of paths to search when importing modules. "
          "Defaults to $ICARUS_MODULE_PATH.");
ABSL_FLAG(std::vector<std::string>, implicitly_embedded_modules, {},
          "Comma-separated list of modules that are embedded implicitly.");
ABSL_FLAG(std::string, byte_code, "",
          "Specifies that code should be compiled to byte-code");
ABSL_FLAG(std::string, object_file, "",
          "Specifies that code should be compiled to byte-code");
ABSL_FLAG(std::string, module_map, "",
          "Filename holding information about the module-map describing the "
          "location precompiled modules");
ABSL_FLAG(std::string, diagnostics, "console",
          "Indicates how diagnostics should be emitted. Options: console "
          "(default), or json.");
ABSL_FLAG(std::string, module_identifier, "",
          "Identifier to be used to uniquely identify this module amongst all "
          "modules being linked together, and must not begin with a tilde (~) "
          "character.");

namespace compiler {
namespace {

struct InvalidTargetTriple {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid target-triple. %s", message));
  }

  std::string message;
};

int Compile(char const *file_name, std::string module_identifier,
            std::string const &output_byte_code,
            std::string const &output_object_file) {
  frontend::SourceIndexer source_indexer;
  auto diag =
      compiler::DiagnosticConsumerFromFlag(FLAGS_diagnostics, source_indexer);
  if (not diag.ok()) {
    std::cerr << diag.status().message();
    return 1;
  }

  auto module_map = module::BazelModuleMap(absl::GetFlag(FLAGS_module_map),
                                           absl::GetFlag(FLAGS_module_paths));
  if (not module_map.ok()) {
    std::cerr << "Failed to initailize module map.\n";
    return 1;
  }

  compiler::WorkSet work_set;
  module::SharedContext shared_context(MakeBuiltinModule());
  compiler::FileImporter importer(
      &work_set, diag->get(), &source_indexer, *std::move(module_map),
      absl::GetFlag(FLAGS_module_paths), shared_context);

  if (not importer.SetImplicitlyEmbeddedModules(
          absl::GetFlag(FLAGS_implicitly_embedded_modules))) {
    return 1;
  }

  auto content = LoadFileContent(file_name);
  if (not content.ok()) {
    (*diag)->Consume(
        MissingModule{.source    = file_name,
                      .requestor = "",
                      .reason    = std::string(content.status().message())});
    return 1;
  }

  auto [mod_id, exec_mod] =
      shared_context.module_table().add_module<compiler::CompiledModule>(
          std::move(module_identifier));
  for (ir::ModuleId embedded_id : importer.implicitly_embedded_modules()) {
    exec_mod->scope().embed(&importer.get(embedded_id));
  }

  std::string_view file_content =
      source_indexer.insert(mod_id, *std::move(content));

  auto parsed_nodes = frontend::Parse(file_content, **diag);
  auto nodes = exec_mod->insert(parsed_nodes.begin(), parsed_nodes.end());

  compiler::PersistentResources resources{
      .work                = &work_set,
      .module              = exec_mod,
      .diagnostic_consumer = diag->get(),
      .importer            = &importer,
      .shared_context      = &shared_context,
  };
  auto main_fn = CompileModule(exec_mod->context(), resources, nodes);

  if ((*diag)->num_consumed() != 0) { return 1; }
  if (not output_byte_code.empty()) {
    auto proto = exec_mod->ToProto();
    auto &mods = *proto.mutable_modules();
    for (auto const &[name, id] : shared_context.module_table().ids()) {
      mods[id.value()] = std::string(name);
    }

    LOG("proto", "%s", proto.DebugString());

    std::string s;
    proto.SerializeToString(&s);
    std::ofstream os(absl::GetFlag(FLAGS_byte_code).c_str(),
                     std::ofstream::out);
    os << s;
    os.close();
  }

  return 0;
}

}  // namespace
}  // namespace compiler

bool HelpFilter(std::string_view module) {
  return absl::EndsWith(module, "main.cc");
}

int main(int argc, char *argv[]) {
  // Provide a new default for --module_paths.
  if (char *const env_str = std::getenv("ICARUS_MODULE_PATH")) {
    absl::SetFlag(&FLAGS_module_paths, absl::StrSplit(env_str, ':'));
  }

  absl::FlagsUsageConfig flag_config;
  flag_config.contains_helpshort_flags = &HelpFilter;
  flag_config.contains_help_flags      = &HelpFilter;
  absl::SetFlagsUsageConfig(flag_config);
  absl::SetProgramUsageMessage("Icarus compiler");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::string module_id = absl::GetFlag(FLAGS_module_identifier);
  if (module_id.empty()) {
    std::cerr << "--module_identifier must not be empty.";
    return 1;
  } else if (module_id[0] == '~') {
    std::cerr << "--module_identifier starts with the character '~'. "
                 "Identifiers starting with a tilde are reserved.";
    return 1;
  }

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (std::string_view key : log_keys) { base::EnableLogging(key); }

  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }

  std::string output_byte_code   = absl::GetFlag(FLAGS_byte_code);
  std::string output_object_file = absl::GetFlag(FLAGS_object_file);

  if (output_byte_code.empty() and output_object_file.empty()) {
    std::cerr
        << "At least one of --byte_code or --object_file must be specified.\n";
    return 1;
  }

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file.\n";
    return 1;
  } else if (args.size() > 2) {
    std::cerr << "Provide exactly one positional argument: source file.\n";
    return 1;
  }

  return compiler::Compile(args[1], std::move(module_id), output_byte_code,
                           output_object_file);
}
