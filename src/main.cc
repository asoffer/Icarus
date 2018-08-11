#include <future>
#include "base/container/vector.h"

#include "ast/call.h"
#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/function_literal.h"
#include "ast/statements.h"
#include "backend/exec.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/source.h"
#include "context.h"
#include "init/argon.h"
#include "init/signal.h"
#include "ir/func.h"
#include "module.h"

#ifdef ICARUS_USE_LLVM
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetSelect.h"
#endif // ICARUS_USE_LLVM

namespace debug {
inline bool parser     = false;
inline bool validation = false;
} // namespace debug

const char *output_file_name = "a.out";
base::vector<Source::Name> files;

// TODO sad. don't use a global to do this.
extern IR::Func* main_fn;

namespace backend {
extern void ReplEval(AST::Expression *expr);
}  // namespace backend

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

namespace backend {
void Execute(IR::Func *fn, const base::untyped_buffer &,
             const base::vector<IR::Addr> &ret_slots,
             backend::ExecContext *ctx);
}

int GenerateCode() {
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


int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  Repl repl;
  auto mod = std::make_unique<Module>();
  Context ctx(mod.get());
repl_start : {
  auto stmts = repl.Parse(&ctx);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    goto repl_start;
  }

  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      auto *decl = &stmt->as<AST::Declaration>();
      decl->assign_scope(ctx.mod_->global_.get());
      decl->VerifyType(&ctx);
      decl->Validate(&ctx);
      decl->EmitIR(&ctx);
      if (ctx.num_errors() != 0) {
        ctx.DumpErrors();
        goto repl_start;
      }

    } else if (stmt->is<AST::Expression>()) {
      auto *expr = &stmt->as<AST::Expression>();
      expr->assign_scope(ctx.mod_->global_.get());
      backend::ReplEval(expr);
      fprintf(stderr, "\n");
    } else {
      NOT_YET(*stmt);
    }
  }
  goto repl_start;
}
}

void argon::Usage() {
  ARGON_FLAG("debug-parser")
      << "Step through the parser step-by-step for debugging."
      << [](bool b = true) { debug::parser = b; };

  ARGON_FLAG("repl", "r")                 //
      << "Run the read-eval-print-loop."  //
      << [](bool b = true) { /* TODO */ };

  ARGON_FLAG("validation", "v")
      << "Whether or not to do function pre/post-condition validation at "
         "compile-time."
      << [](bool b = true) { debug::validation = b; };

  ArgonHandleOther = [](const char *arg) { files.emplace_back(arg); };

  bool repl = false;
  ArgonExecute = (repl ? RunRepl : GenerateCode);
}

int main(int argc, char *argv[]) {
  init::InstallSignalHandlers();
  return argon::Parse(argc, argv);
}
