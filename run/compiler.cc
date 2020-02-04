#include <dlfcn.h>
#include <memory>
#include <vector>

#include "base/defer.h"
#include "base/expected.h"
#include "base/untyped_buffer.h"
#include "compiler/compiler.h"
#include "compiler/executable_module.h"
#include "compiler/module.h"
#include "diagnostic/errors.h"
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

  diagnostic::StreamingConsumer diag(stderr);
  auto expected_pending_mod =
      module::ImportModule<compiler::ExecutableModule>(file_name);
  if (not expected_pending_mod) {
    diag.Consume(diagnostic::MissingModule{
        .source    = file_name.value,
        .requestor = "",
    });
  }

  if (diag.num_consumed() > 0) {
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
  exec_mod->main()->WriteByteCode();
  interpretter::ExecutionContext exec_ctx;
  interpretter::Execute(
      exec_mod->main(),
      base::untyped_buffer::MakeFull(exec_mod->main()->num_regs() * 16), {},
      &exec_ctx);

  return 0;
}
