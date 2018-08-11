#include "run/run.h"

#include <future>

#include "backend/exec.h"
#include "base/container/vector.h"
#include "base/guarded.h"
#include "base/source.h"
#include "base/untyped_buffer.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetSelect.h"
#endif // ICARUS_USE_LLVM

namespace backend {
void Execute(IR::Func *fn, const base::untyped_buffer &,
             const base::vector<IR::Addr> &ret_slots,
             backend::ExecContext *ctx);
}

base::vector<Source::Name> files;

// TODO sad. don't use a global to do this.
extern IR::Func* main_fn;

base::guarded<base::unordered_map<Source::Name,
                                 std::shared_future<std::unique_ptr<Module>>>>
    modules;

extern std::atomic<bool> found_errors;

void ScheduleModule(const Source::Name &src) {
  auto handle = modules.lock();
  auto iter = handle->find(src);
  if (iter != handle->end()) { return; }
  handle->emplace(src, std::shared_future<std::unique_ptr<Module>>(std::async(
                           std::launch::async, Module::Compile, src)));
}

int RunCompiler() {
#ifdef ICARUS_USE_LLVM
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
#endif // ICARUS_USE_LLVM
  for (const auto &src : files) { ScheduleModule(src); }

  size_t current_size = 0;
  do {
    base::vector<std::shared_future<std::unique_ptr<Module>> *> future_ptrs;
    {
      auto handle  = modules.lock();
      current_size = handle->size();
      for (auto & [ src, module ] : *handle) { future_ptrs.push_back(&module); }
    }
    for (auto *future : future_ptrs) { future->wait(); }
  } while (current_size != modules.lock()->size());

#ifndef ICARUS_USE_LLVM

    if (main_fn == nullptr) {
      // TODO make this an actual error?
      std::cerr << "No compiled module has a `main` function.\n";
    } else if (!found_errors) {
      backend::ExecContext exec_ctx;
      backend::Execute(main_fn, base::untyped_buffer(0), {}, &exec_ctx);
    }
#endif

  return 0;
}
