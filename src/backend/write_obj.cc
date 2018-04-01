#include <system_error>
#include <iostream>
#include <string>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "../module.h"

namespace backend {
std::string WriteObjectFile(const std::string& name, Module * mod) {
  auto target_triple = llvm::sys::getDefaultTargetTriple();
  mod->llvm_->setTargetTriple(target_triple);
  std::string err;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);
  if (target == nullptr) { return err; }

  auto target_machine = target->createTargetMachine(
      target_triple, "generic", "", llvm::TargetOptions{},
      llvm::Optional<llvm::Reloc::Model>());
  mod->llvm_->setDataLayout(target_machine->createDataLayout());

  std::error_code err_code;
  llvm::raw_fd_ostream dest(name, err_code,
                            llvm::sys::fs::OpenFlags::F_None);
  if (err_code) { std::cerr << err_code.message(); }

  llvm::legacy::PassManager pass;
  if (target_machine->addPassesToEmitFile(
          pass, dest, llvm::TargetMachine::CGFT_ObjectFile)) {
    return "TheTargetMachine can't emit a file of this type";
  }
  pass.run(*mod->llvm_);
  dest.flush();
  return "";
}
}  // namespace backend
