#include <dlfcn.h>
#include <filesystem>
#include <vector>

#include "backend/exec.h"
#include "base/expected.h"
#include "base/untyped_buffer.h"
#include "base/util.h"
#include "core/pending_module.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
#include "opt/combine_blocks.h"

std::vector<std::string> files;

// TODO sad. don't use a global to do this.
extern ir::CompiledFn *main_fn;

extern std::atomic<bool> found_errors;

Module *CompileModule(Module *mod, std::filesystem::path const *path);

int RunCompiler() {
  void *libc_handle = dlopen("/lib/x86_64-linux-gnu/libc.so.6", RTLD_LAZY);
  ASSERT(libc_handle != nullptr);
  base::defer d([libc_handle] { dlclose(libc_handle); });

  error::Log log;
  for (const auto &src : files) {
    if (!core::ImportModule(std::filesystem::path{src},
                            std::filesystem::path{""}, CompileModule)) {
      log.MissingModule(src, "");
    }
  }

  if (log.size() > 0) {
    log.Dump();
    return 0;
  }

  core::AwaitAllModulesTransitively();

  if (main_fn == nullptr) {
    // TODO make this an actual error?
    std::cerr << "No compiled module has a `main` function.\n";
  } else if (!found_errors) {
    ASSERT(main_fn->mod_ != nullptr);

    // TODO All the functions? In all the modules?
    opt::CombineBlocks(main_fn);

    backend::ExecContext exec_ctx;
    backend::Execute(main_fn, base::untyped_buffer(0), {}, &exec_ctx);
  }

  return 0;
}
