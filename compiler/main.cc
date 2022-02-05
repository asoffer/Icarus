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
#include "backend/llvm.h"
#include "base/log.h"
#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "compiler/importer.h"
#include "compiler/module.h"
#include "compiler/work_graph.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "ir/interpreter/execution_context.h"
#include "ir/subroutine.h"
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
ABSL_FLAG(std::vector<std::string>, implicitly_embedded_modules, {},
          "Comma-separated list of modules that are embedded implicitly.");
ABSL_FLAG(std::string, byte_code, "",
          "Specifies that code should be compiled to byte-code");
ABSL_FLAG(std::string, object_file, "",
          "Specifies that code should be compiled to byte-code");
ABSL_FLAG(std::string, module_map, "",
          "Filename holding information about the module-map describing the "
          "location precompiled modules");

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

llvm::TargetMachine *InitializeLlvm(diagnostic::DiagnosticConsumer &diag) {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  auto target_triple = llvm::sys::getDefaultTargetTriple();
  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
  if (not target) {
    diag.Consume(InvalidTargetTriple{.message = std::move(error)});
    return nullptr;
  }

  char const cpu[]      = "generic";
  char const features[] = "";
  llvm::TargetOptions target_options;
  llvm::Optional<llvm::Reloc::Model> relocation_model;
  return target->createTargetMachine(target_triple, cpu, features,
                                     target_options, relocation_model);
}

int CompileToObjectFile(CompiledModule const &module, ir::Subroutine const &fn,
                        llvm::TargetMachine *target_machine) {
  llvm::LLVMContext context;
  llvm::Module llvm_module("module", context);
  llvm_module.setDataLayout(target_machine->createDataLayout());

  std::error_code error_code;
  llvm::raw_fd_ostream destination(absl::GetFlag(FLAGS_object_file), error_code,
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

int Compile(char const *file_name, std::string const &output_byte_code,
            std::string const &output_object_file) {
  frontend::SourceIndexer source_indexer;
  diagnostic::StreamingConsumer diag(stderr, &source_indexer);

  llvm::TargetMachine *target_machine;
  if (not output_object_file.empty()) {
    target_machine = InitializeLlvm(diag);
    if (not target_machine) { return 1; }
  }

  auto module_map = MakeModuleMap(absl::GetFlag(FLAGS_module_map));
  if (not module_map) {
    std::cerr << "Failed to initailize module map.\n"; return 1;
  }

  compiler::WorkSet work_set;
  compiler::FileImporter importer(&work_set, &diag, &source_indexer,
                                  *std::move(module_map),
                                  absl::GetFlag(FLAGS_module_paths));

  if (not importer.SetImplicitlyEmbeddedModules(
          absl::GetFlag(FLAGS_implicitly_embedded_modules))) {
    return 1;
  }

  auto content = LoadFileContent(file_name);
  if (not content.ok()) {
    diag.Consume(
        MissingModule{.source    = file_name,
                      .requestor = "",
                      .reason    = std::string(content.status().message())});
    return 1;
  }

  std::string_view file_content =
      source_indexer.insert(ir::ModuleId::New(), *std::move(content));

  ir::Module ir_module;
  compiler::Context context(&ir_module);
  compiler::CompiledModule exec_mod(file_content, &context);
  for (ir::ModuleId embedded_id : importer.implicitly_embedded_modules()) {
    exec_mod.scope().embed(&importer.get(embedded_id));
  }

  auto parsed_nodes = frontend::Parse(file_content, diag);
  auto nodes        = exec_mod.insert(parsed_nodes.begin(), parsed_nodes.end());

  compiler::PersistentResources resources{
      .work                = &work_set,
      .module              = &exec_mod,
      .diagnostic_consumer = &diag,
      .importer            = &importer,
  };
  auto main_fn = CompileModule(context, resources, nodes);

  if (diag.num_consumed() != 0) { return 1; }
  if (not output_byte_code.empty()) {
    std::string s;
    module::ModuleWriter w(&s);
    base::Serialize(w, exec_mod);
    std::ofstream os(absl::GetFlag(FLAGS_byte_code).c_str(), std::ofstream::out);
    os << s;
    os.close();
  }

  int return_code = 0;
  if (not output_object_file.empty()) {
    return_code = CompileToObjectFile(exec_mod, *main_fn, target_machine);
  }

  return return_code;
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

  return compiler::Compile(args[1], output_byte_code, output_object_file);
}
