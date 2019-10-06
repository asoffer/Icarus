#include <dlfcn.h>
#include <filesystem>
#include <vector>

#include "backend/exec.h"
#include "base/expected.h"
#include "base/untyped_buffer.h"
#include "base/util.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "module/pending.h"
#include "opt/combine_blocks.h"

std::vector<std::string> files;

// TODO sad. don't use a global to do this.
extern ir::CompiledFn *main_fn;

extern std::atomic<bool> found_errors;

std::unique_ptr<module::Module> CompileModule(frontend::Source *);

int RunCompiler() {
  void *libc_handle = dlopen("/lib/x86_64-linux-gnu/libc.so.6", RTLD_LAZY);
  ASSERT(libc_handle != nullptr);
  base::defer d([libc_handle] { dlclose(libc_handle); });

  error::Log log;
  for (const auto &src : files) {
    if (!module::ImportModule(std::filesystem::path{src}, nullptr,
                              CompileModule)) {
      log.MissingModule(src, "");
    }
  }

  if (log.size() > 0) {
    log.Dump();
    return 0;
  }

  module::AwaitAllModulesTransitively();

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
