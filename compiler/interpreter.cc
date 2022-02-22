#include <dlfcn.h>

#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "absl/cleanup/cleanup.h"
#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/flags/usage.h"
#include "absl/flags/usage_config.h"
#include "absl/strings/match.h"
#include "absl/strings/str_split.h"
#include "absl/types/span.h"
#include "base/log.h"
#include "base/untyped_buffer.h"
#include "compiler/importer.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/json.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "ir/interpreter/evaluate.h"
#include "ir/subroutine.h"
#include "module/module.h"
#include "module/shared_context.h"
#include "nlohmann/json.hpp"
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
ABSL_FLAG(std::string, module_map, "",
          "Filename holding information about the module-map describing the "
          "location precompiled modules");
ABSL_FLAG(std::string, diagnostics, "console",
          "Indicates how diagnostics should be emitted. Options: console "
          "(default), or json.");
namespace compiler {
namespace {

int Interpret(char const *file_name, absl::Span<char *> program_arguments,
              frontend::SourceIndexer &source_indexer,
              diagnostic::DiagnosticConsumer &diag) {
  auto content = LoadFileContent(file_name);
  if (not content.ok()) {
    diag.Consume(
        MissingModule{.source    = file_name,
                      .requestor = "",
                      .reason    = std::string(content.status().message())});
    return 1;
  }

  std::string module_map_file = absl::GetFlag(FLAGS_module_map);
  auto module_map             = MakeModuleMap(module_map_file);
  if (not module_map) {
    diag.Consume(MissingModuleMap{
        .module_map = std::move(module_map_file),
    });
    return 1;
  }

  compiler::WorkSet work_set;
  module::SharedContext shared_context;
  compiler::FileImporter importer(
      &work_set, &diag, &source_indexer, *std::move(module_map),
      absl::GetFlag(FLAGS_module_paths), shared_context);

  if (not importer.SetImplicitlyEmbeddedModules(
          absl::GetFlag(FLAGS_implicitly_embedded_modules))) {
    return 1;
  }

  std::string_view file_content =
      source_indexer.insert(ir::ModuleId::New(), *std::move(content));

  ir::Module ir_module;
  Context context(&ir_module);
  CompiledModule exec_mod(file_content, &context);
  for (ir::ModuleId embedded_id : importer.implicitly_embedded_modules()) {
    exec_mod.scope().embed(&importer.get(embedded_id));
  }

  PersistentResources resources{
      .work                = &work_set,
      .module              = &exec_mod,
      .diagnostic_consumer = &diag,
      .importer            = &importer,
      .shared_context      = &shared_context,
  };

  auto parsed_nodes = frontend::Parse(file_content, diag);
  auto nodes        = exec_mod.insert(parsed_nodes.begin(), parsed_nodes.end());
  ASSIGN_OR(return 1,  //
                   auto main_fn, CompileModule(context, resources, nodes));
  // TODO All the functions? In all the modules?
  if (absl::GetFlag(FLAGS_opt_ir)) { opt::RunAllOptimizations(&main_fn); }

  std::vector<ir::Slice> arguments;
  for (char *argument : program_arguments) {
    arguments.emplace_back(reinterpret_cast<ir::addr_t>(argument),
                           strlen(argument));
  }
  ir::Slice argument_slice(
      reinterpret_cast<ir::addr_t>(const_cast<ir::Slice *>(arguments.data())),
      arguments.size());
  ir::CompleteResultBuffer argument_buffer;
  argument_buffer.append(&argument_slice);

  importer.set_subroutine(&exec_mod, std::move(main_fn));
  importer.ForEachSubroutine([&](ir::Subroutine const &subroutine) {
    InterpretAtCompileTime(subroutine, argument_buffer);
  });

  return 0;
}

}  // namespace
}  // namespace compiler

namespace {

enum class DiagnosticOutputKind {
  Console,
  Json,
};

absl::StatusOr<DiagnosticOutputKind> GetDiagnosticOutputKind() {
  std::string diagnostics = absl::GetFlag(FLAGS_diagnostics);
  if (diagnostics == "console") { return DiagnosticOutputKind::Console; }
  if (diagnostics == "json") { return DiagnosticOutputKind::Json; }
  return absl::InvalidArgumentError(
      absl::StrFormat("Invalid value for --diagnostics flag '%s'. Valid values "
                      "are 'console' or 'json'\n",
                      diagnostics));
}

}  // namespace

bool HelpFilter(std::string_view module) {
  return absl::EndsWith(module, "/interpreter.cc");
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
  absl::SetProgramUsageMessage("the Icarus interpreter.");
  std::vector<char *> args = absl::ParseCommandLine(argc, argv);
  absl::InitializeSymbolizer(args[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (std::string_view key : log_keys) { base::EnableLogging(key); }

  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }

  auto kind = GetDiagnosticOutputKind();
  if (not kind.ok()) {
    std::cerr << kind.status().message();
    return 1;
  }

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file";
    return 1;
  }

  absl::Span<char *> arguments = absl::MakeSpan(args).subspan(2);

  frontend::SourceIndexer source_indexer;
  switch (*kind) {
    case DiagnosticOutputKind::Console: {
      diagnostic::StreamingConsumer diag(stderr, &source_indexer);
      return compiler::Interpret(args[1], arguments, source_indexer, diag);
    }
    case DiagnosticOutputKind::Json: {
      nlohmann::json json;
      absl::Cleanup c = [&] {
        if (not json.empty()) { std::cerr << json.dump(2); }
      };
      diagnostic::JsonConsumer diag(source_indexer, json);
      return compiler::Interpret(args[1], arguments, source_indexer, diag);
    }
    default: UNREACHABLE();
  }
}
