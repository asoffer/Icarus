#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/MC/SubTargetFeature.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

void WriteObjectFile(const char *out_file) {
  RUN(timer, "LLVM") {
    std::error_code EC;
    llvm::raw_fd_ostream destination(out_file, EC, llvm::sys::fs::F_None);
    if (EC) { assert(false && "Not yet implemented: Write error handling."); }

    llvm::legacy::PassManager pass;

    assert(!target_machine->addPassesToEmitFile(
               pass, destination, llvm::TargetMachine::CGFT_ObjectFile) &&
           "TargetMachine can't emit a file of this type");

    pass.run(*global_module);

    destination.flush();
  }
}

// TODO this initialization can be done asynchronosly. We don't touch LLVM
// initially.
void InitializeLLVM() {
  RUN(timer, "LLVM initialization") {
    // TODO Assuming X86 architecture. If a command-line arg says otherwise,
    // load the appropriate tools
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86AsmParser();

    llvm::StringMap<bool> host_features;
    llvm::SubtargetFeatures features;
    if (llvm::sys::getHostCPUFeatures(host_features)) {
      for (auto &feat : host_features) {
        features.AddFeature(feat.first(), feat.second);
      }
    }

    llvm::TargetOptions opt;
    std::string error = "";
    auto target = llvm::TargetRegistry::lookupTarget(
        llvm::sys::getDefaultTargetTriple(), error);
    if (error != "") {
      std::cerr << error << std::endl;
      assert(false && "Error in target string lookup");
    }

    // Hack to get the right target triple on my system:
    std::string triple_string = llvm::sys::getDefaultTargetTriple();
    auto last_pos             = triple_string.rfind("15");
    triple_string             = triple_string.substr(0, last_pos) + "10.11";

    target_machine = target->createTargetMachine(
        triple_string, llvm::sys::getHostCPUName(), features.getString(), opt);
    assert(target_machine);

    // Init global module, function, etc.
    global_module = new llvm::Module("global_module", llvm::getGlobalContext());
    global_module->setDataLayout(target_machine->createDataLayout());
  }
}
