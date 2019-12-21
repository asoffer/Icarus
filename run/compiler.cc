#include <dlfcn.h>
#include <memory>
#include <vector>

#include "base/expected.h"
#include "base/untyped_buffer.h"
#include "base/util.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "error/log.h"
#include "frontend/parse.h"
#include "frontend/source/file_name.h"
#include "interpretter/execute.h"
#include "ir/compiled_fn.h"
#include "module/module.h"
#include "module/pending.h"
#include "opt/opt.h"

int RunCompiler(frontend::FileName const &file_name) {
  void *libc_handle = dlopen("/lib/x86_64-linux-gnu/libc.so.6", RTLD_LAZY);
  ASSERT(libc_handle != nullptr);
  base::defer d([libc_handle] { dlclose(libc_handle); });

  error::Log log;
  auto expected_pending_mod = module::ImportModule(
      file_name, nullptr, compiler::CompileExecutableModule);
  if (not expected_pending_mod) { log.MissingModule(file_name.value, ""); }

  if (log.size() > 0) {
    log.Dump();
    return 0;
  }

  module::AwaitAllModulesTransitively();
  // TODO remove reinterpret_cast
  auto *exec_mod = reinterpret_cast<compiler::ExecutableModule *>(
      expected_pending_mod->get());

  if (not exec_mod->main()) {
    // TODO make this an actual error?
    std::cerr << "No compiled module has a `main` function.\n";
  }

  // TODO All the functions? In all the modules?
  opt::RunAllOptimizations(exec_mod->main());
  interpretter::ExecutionContext exec_ctx;
  interpretter::Execute(exec_mod->main(), base::untyped_buffer(0), {},
                        &exec_ctx);

  return 0;
}
