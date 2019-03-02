#include <dlfcn.h>
#include <filesystem>

#include "backend/exec.h"
#include <vector>
#include "base/untyped_buffer.h"
#include "base/util.h"
#include "ir/func.h"
#include "misc/context.h"
#include "misc/module.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/ADT/STLExtras.h"
#include "llvm/ir/Module.h"
#include "llvm/Support/TargetSelect.h"
#endif  // ICARUS_USE_LLVM

std::vector<std::string> files;

// TODO sad. don't use a global to do this.
extern ir::Func *main_fn;

extern std::atomic<bool> found_errors;

int RunCompiler() {
  void *libc_handle = dlopen("/lib/x86_64-linux-gnu/libc.so.6", RTLD_LAZY);
  ASSERT(libc_handle != nullptr);
  base::defer d([libc_handle] { dlclose(libc_handle); });

#ifdef ICARUS_USE_LLVM
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
#endif  // ICARUS_USE_LLVM

  error::Log log;
  for (const auto &src : files) {
    Module::Schedule(&log, std::filesystem::path{src});
  }

  if (log.size() > 0) {
    log.Dump();
    return 0;
  }

  AwaitAllModulesTransitively();

#ifndef ICARUS_USE_LLVM

  if (main_fn == nullptr) {
    // TODO make this an actual error?
    std::cerr << "No compiled module has a `main` function.\n";
  } else if (!found_errors) {
    ASSERT(main_fn->mod_ != nullptr);
    backend::ExecContext exec_ctx;
    backend::Execute(main_fn, base::untyped_buffer(0), {}, &exec_ctx);
  }
#endif

  return 0;
}
