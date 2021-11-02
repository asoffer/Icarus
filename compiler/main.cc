#include <cstdlib>
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
#include "backend/llvm.h"
#include "base/log.h"
#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "compiler/module.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "frontend/source/shared.h"
#include "ir/compiled_fn.h"
#include "ir/interpreter/execution_context.h"
#include "llvm/ADT/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
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
ABSL_FLAG(
    std::string, output, "",
    "Name of the output file to which the generated output should be written.");
ABSL_FLAG(std::vector<std::string>, implicitly_embedded_modules, {},
          "Comma-separated list of modules that are embedded implicitly.");

namespace compiler {
namespace {

struct InvalidTargetTriple {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Invalid target-triple. %s", message));
  }

  std::string message;
};

int CompileToObjectFile(CompiledModule const &module, ir::CompiledFn const &fn,
                        llvm::TargetMachine *target_machine) {
  llvm::LLVMContext context;
  llvm::Module llvm_module("module", context);
  llvm_module.setDataLayout(target_machine->createDataLayout());

  std::error_code error_code;
  llvm::raw_fd_ostream destination(absl::GetFlag(FLAGS_output), error_code,
                                   llvm::sys::fs::OF_None);
  llvm::legacy::PassManager pass;
  auto file_type = llvm::LLVMTargetMachine::CGFT_ObjectFile;

  if (target_machine->addPassesToEmitFile(pass, destination, nullptr,
                                          file_type)) {
    // TODO: Add a diagnostic: The target machine cannot emit a file of this
    // type.
    std::cerr << "Failed";
    return 1;
  }

  llvm::IRBuilder<> builder(context);

  backend::LlvmEmitter emitter(builder, &llvm_module);

  emitter.EmitModule(module);
  auto *f = emitter.EmitFunction(&fn, module::Linkage::External);
  f->setName("main");

  pass.run(llvm_module);

  destination.flush();
  return 0;
}

int Compile(frontend::FileName const &file_name) {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  diagnostic::StreamingConsumer diag(stderr, frontend::SharedSource());

  auto target_triple = llvm::sys::getDefaultTargetTriple();
  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
  if (not target) {
    diag.Consume(InvalidTargetTriple{.message = std::move(error)});
    return 1;
  }

  auto canonical_file_name = frontend::CanonicalFileName::Make(file_name);
  auto maybe_file_src      = frontend::FileSource::Make(canonical_file_name);
  if (not maybe_file_src.ok()) {
    diag.Consume(frontend::MissingModule{
        .source    = canonical_file_name,
        .requestor = "",
        .reason    = stringify(maybe_file_src),
    });
    return 1;
  }

  char const cpu[]      = "generic";
  char const features[] = "";
  llvm::TargetOptions target_options;
  llvm::Optional<llvm::Reloc::Model> relocation_model;
  llvm::TargetMachine *target_machine = target->createTargetMachine(
      target_triple, cpu, features, target_options, relocation_model);

  auto *src = &*maybe_file_src;
  diag      = diagnostic::StreamingConsumer(stderr, src);
  module::FileImporter<compiler::CompiledModule> importer(
      [&](compiler::CompiledModule *mod, base::PtrSpan<ast::Node const> nodes) {
        PersistentResources resources{
            .diagnostic_consumer = &mod->diagnostic_consumer(),
            .importer            = &importer};
        CompileLibrary(mod->context(mod), resources, std::move(nodes));
      });

  importer.module_lookup_paths = absl::GetFlag(FLAGS_module_paths);
  if (not importer.SetImplicitlyEmbeddedModules(
          absl::GetFlag(FLAGS_implicitly_embedded_modules))) {
    return 1;
  }

  compiler::CompiledModule exec_mod;
  exec_mod.set_diagnostic_consumer<diagnostic::StreamingConsumer>(stderr, src);
  for (ir::ModuleId embedded_id : importer.implicitly_embedded_modules()) {
    exec_mod.embed(importer.get(embedded_id));
  }

  compiler::PersistentResources resources{.diagnostic_consumer = &diag,
                                          .importer            = &importer};

  auto nodes = exec_mod.InitializeNodes(frontend::Parse(src->buffer(), diag));
  auto main_fn =
      CompileExecutable(exec_mod.context(&exec_mod), resources, nodes);
  return CompileToObjectFile(exec_mod, main_fn, target_machine);
}

}  // namespace
}  // namespace compiler

bool HelpFilter(absl::string_view module) {
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

  if (absl::GetFlag(FLAGS_output).empty()) {
    std::cerr << "--output must be specified.";
    return 1;
  }

  std::vector<std::string> log_keys = absl::GetFlag(FLAGS_log);
  for (absl::string_view key : log_keys) { base::EnableLogging(key); }

  if (std::string lib = absl::GetFlag(FLAGS_link); not lib.empty()) {
    ASSERT_NOT_NULL(dlopen(lib.c_str(), RTLD_LAZY));
  }

  if (args.size() < 2) {
    std::cerr << "Missing required positional argument: source file"
              << std::endl;
    return 1;
  }
  int return_code = 0;
  for (int i = 1; i < args.size(); ++i) {
    return_code += compiler::Compile(frontend::FileName(args[i]));
  }
  return return_code;
}
