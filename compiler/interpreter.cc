#include <dlfcn.h>

#include <cstdlib>
#include <memory>
#include <string>
#include <string_view>
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
#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "compiler/importer.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file.h"
#include "frontend/source/file_name.h"
#include "ir/subroutine.h"
#include "ir/interpreter/evaluate.h"
#include "module/module.h"
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

namespace compiler {
namespace {

int Interpret(frontend::FileName const &file_name) {
  diagnostic::StreamingConsumer diag(stderr, nullptr);
  auto canonical_file_name = frontend::CanonicalFileName::Make(file_name);
  auto maybe_file_src = frontend::SourceBufferFromFile(canonical_file_name);
  if (not maybe_file_src.ok()) {
    diag.Consume(frontend::MissingModule{
        .source    = canonical_file_name,
        .requestor = "",
        .reason    = std::string(maybe_file_src.status().message())});
    return 1;
  }

  auto *src = &*maybe_file_src;
  diag      = diagnostic::StreamingConsumer(stderr, src);

  WorkSet work_set;
  FileImporter importer(&work_set, &diag, absl::GetFlag(FLAGS_module_paths));
  if (not importer.SetImplicitlyEmbeddedModules(
          absl::GetFlag(FLAGS_implicitly_embedded_modules))) {
    return 1;
  }

  ir::Module ir_module;
  Context context(&ir_module);
  CompiledModule exec_mod(src, &context);
  for (ir::ModuleId embedded_id : importer.implicitly_embedded_modules()) {
    exec_mod.scope().embed(&importer.get(embedded_id));
  }

  PersistentResources resources{
      .work                = &work_set,
      .module              = &exec_mod,
      .diagnostic_consumer = &diag,
      .importer            = &importer,
  };

  auto parsed_nodes = frontend::Parse(*src, diag);
  auto nodes        = exec_mod.insert(parsed_nodes.begin(), parsed_nodes.end());
  ASSIGN_OR(return 1,  //
                   auto main_fn, CompileModule(context, resources, nodes));
  // TODO All the functions? In all the modules?
  if (absl::GetFlag(FLAGS_opt_ir)) { opt::RunAllOptimizations(&main_fn); }

  importer.set_subroutine(&exec_mod, std::move(main_fn));
  importer.ForEachSubroutine([&](ir::Subroutine const &subroutine) {
    InterpretAtCompileTime(subroutine);
  });

  return 0;
}

}  // namespace
}  // namespace compiler

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

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  if (args.size() > 2) {
    std::cerr << "Too many positional arguments." << std::endl;
    return 1;
  }

  return compiler::Interpret(frontend::FileName(args[1]));
}
