#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "absl/strings/str_split.h"
#include "backend/function.h"
#include "backend/type.h"
#include "base/expected.h"
#include "base/log.h"
#include "base/no_destructor.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/consumer/streaming.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "frontend/source/shared.h"
#include "init/cli.h"
#include "ir/compiled_fn.h"
#include "ir/interpretter/execution_context.h"
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

namespace debug {
extern bool parser;
extern bool validation;
extern bool optimize_ir;
}  // namespace debug

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

int CompileToObjectFile(ExecutableModule const &module,
                        llvm::TargetMachine *target_machine) {
  llvm::LLVMContext context;
  llvm::Module llvm_module("module", context);
  llvm_module.setDataLayout(target_machine->createDataLayout());

  // TODO: Lift this to a flag.
  char const filename[] = "a.out";
  std::error_code error_code;
  llvm::raw_fd_ostream destination(filename, error_code,
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

  // Declare all functions first so we can safely iterate through each and
  // emit instructions. Doing so might end up calling functions, so first
  // emiting declarations guarantees they're already available.
  absl::flat_hash_map<ir::CompiledFn const *, llvm::Function *> llvm_fn_map;
  module.context().ForEachCompiledFn([&](ir::CompiledFn const *fn) {
    llvm_fn_map.emplace(fn,
                        backend::DeclareLlvmFunction(*fn, module, llvm_module));
  });
  llvm_fn_map.emplace(
      &module.main(),
      llvm::Function::Create(
          llvm::cast<llvm::FunctionType>(backend::ToLlvmType(
              module.main().type(), llvm_module.getContext())),
          llvm::Function::ExternalLinkage, "main", &llvm_module));

  llvm::IRBuilder<> builder(context);
  module.context().ForEachCompiledFn([&](ir::CompiledFn const *fn) {
    backend::EmitLlvmFunction(builder, context, *fn, *llvm_fn_map.at(fn));
  });
  backend::EmitLlvmFunction(builder, context, module.main(),
                            *llvm_fn_map.at(&module.main()));

  llvm_module.dump();

  // pass.run(llvm_module);

  // destination.flush();
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
  if (not maybe_file_src) {
    diag.Consume(frontend::MissingModule{.source    = canonical_file_name,
                                         .requestor = ""});
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
  compiler::ExecutableModule exec_mod;
  exec_mod.AppendNodes(frontend::Parse(*src, diag), diag);
  if (diag.num_consumed() != 0) { return 1; }

  return CompileToObjectFile(exec_mod, target_machine);
}

}  // namespace
}  // namespace compiler

void cli::Usage() {
  static base::NoDestructor<frontend::FileName> file;
  execute = [] { return compiler::Compile(*file); };

  Flag("help") << "Show usage information."
               << []() { execute = cli::ShowUsage; };

#if defined(ICARUS_DEBUG)
  Flag("debug-parser") << "Step through the parser step-by-step for debugging."
                       << [](bool b = false) { debug::parser = b; };

  Flag("opt-ir") << "Opmitize intermediate representation"
                 << [](bool b = false) { debug::optimize_ir = b; };

  Flag("log") << "Comma-separated list of log keys" << [](char const *keys) {
    for (std::string_view key : absl::StrSplit(keys, ',')) {
      base::EnableLogging(key);
    }
  };
#endif  // defined(ICARUS_DEBUG)

  Flag("link") << "Library to be dynamically loaded by the compiler to be used "
                  "at compile-time. Libraries will not be unloaded."
               << [](char const *lib) {
                    static_cast<void>(ASSERT_NOT_NULL(dlopen(lib, RTLD_LAZY)));
                  };

  HandleOther = [](char const *arg) { *file = frontend::FileName(arg); };
}

int main(int argc, char *argv[]) {
  absl::InitializeSymbolizer(argv[0]);
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);
  return cli::ParseAndRun(argc, argv);
}
