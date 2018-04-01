#include <cstring>
#include <future>
#include <system_error>
#include <vector>

#include "ast/ast.h"
#include "ast/statements.h"
#include "backend/emit.h"
#include "base/debug.h"
#include "base/guarded.h"
#include "base/source.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "module.h"
#include "util/command_line_args.h"
#include "util/timer.h"

Timer timer;

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

extern void ReplEval(AST::Expression *expr);

base::guarded<std::unordered_map<Source::Name,
                                 std::shared_future<std::unique_ptr<Module>>>>
    modules;
// TODO deprecate source_map
std::unordered_map<Source::Name, File *> source_map;
void ScheduleModule(const Source::Name &src) {
  auto handle = modules.lock();
  auto iter = handle->find(src);
  if (iter != handle->end()) { return; }
  handle->emplace(src, std::shared_future<std::unique_ptr<Module>>(
                           std::async(std::launch::async, [src]() {
                             Context ctx;
                             auto *f = new File(src);
                             source_map[src] = f;
                             error::Log log;
                             auto file_stmts = f->Parse(&log);
                             if (log.size() > 0) {
                               log.Dump();
                               return std::move(ctx.mod_);
                             }

                             file_stmts->assign_scope(ctx.mod_->global_.get());
                             file_stmts->Validate(&ctx);
                             if (ctx.num_errors() != 0) {
                               ctx.DumpErrors();
                               return std::move(ctx.mod_);
                             }

                             file_stmts->EmitIR(&ctx);
                             if (ctx.num_errors() != 0) {
                               ctx.DumpErrors();
                               return std::move(ctx.mod_);
                             }

                             ctx.mod_->statements_ = std::move(*file_stmts);
                             backend::EmitAll(ctx.mod_->fns_,
                                              ctx.mod_->llvm_.get());

                             for (const auto &stmt :
                                  ctx.mod_->statements_.content_) {
                               if (!stmt->is<AST::Declaration>()) { continue; }
                               auto &decl = stmt->as<AST::Declaration>();
                               if (decl.identifier->token != "main") {
                                 continue;
                               }
                               auto val = Evaluate(decl.init_val.get(), &ctx);
                               ASSERT_EQ(val.size(), 1u);
                               auto fn_lit = std::get<AST::FunctionLiteral *>(
                                   val[0].value);
                               // TODO check more than one?

                               fn_lit->ir_func_->llvm_fn_->setName("main");
                               fn_lit->ir_func_->llvm_fn_->setLinkage(
                                   llvm::GlobalValue::ExternalLinkage);
                             }

                             llvm ::InitializeAllTargetInfos();
                             llvm ::InitializeAllTargets();
                             llvm ::InitializeAllTargetMCs();
                             llvm ::InitializeAllAsmParsers();
                             llvm ::InitializeAllAsmPrinters();

                             auto target_triple =
                                 llvm::sys::getDefaultTargetTriple();
                             ctx.mod_->llvm_->setTargetTriple(target_triple);
                             std::string err;
                             auto target = llvm::TargetRegistry::lookupTarget(
                                 target_triple, err);
                             if (target == nullptr) {
                               std::cerr << err;
                               return std::move(ctx.mod_);
                             }

                             auto target_machine = target->createTargetMachine(
                                 target_triple, "generic", "",
                                 llvm::TargetOptions{},
                                 llvm::Optional<llvm::Reloc::Model>());
                             ctx.mod_->llvm_->setDataLayout(
                                 target_machine->createDataLayout());

                             std::error_code err_code;
                             llvm::raw_fd_ostream dest(
                                 src.substr(0, src.size() - 2) + "o", err_code,
                                 llvm::sys::fs::OpenFlags::F_None);
                             if (err_code) {
                               std::cerr << err_code.message();
                             }

                             llvm::legacy::PassManager pass;
                             if (target_machine->addPassesToEmitFile(
                                     pass, dest,
                                     llvm::TargetMachine::CGFT_ObjectFile)) {
                               std::cerr << "TheTargetMachine can't emit a "
                                            "file of this type";
                             }
                             pass.run(*ctx.mod_->llvm_);
                             dest.flush();
                             return std::move(ctx.mod_);
                           })));
}

int GenerateCode() {
  for (const auto &src : files) { ScheduleModule(src); }

  size_t current_size = 0;
  do {
    std::vector<std::shared_future<std::unique_ptr<Module>> *> future_ptrs;
    {
      auto handle = modules.lock();
      current_size = handle->size();
      for (auto & [ src, module ] : *handle) { future_ptrs.push_back(&module); }
    }
    for (auto *future : future_ptrs) { future->wait(); }
  } while (current_size != modules.lock()->size());

  /*
  Context ctx;
  RUN(timer, "Verify preconditions") {
    std::queue<IR::Func *> validation_queue;
    for (const auto &fn : ctx.mod_->fns_) { validation_queue.push(fn.get()); }

    int num_errors = 0;
    while (!validation_queue.empty()) {
      auto fn = std::move(validation_queue.front());
      validation_queue.pop();
      num_errors += fn->ValidateCalls(&validation_queue);
    }
  }
  */

  // Tag main
  /*
*/

  return 0;
}

int RunRepl() {
  std::puts("Icarus REPL (v0.1)");

  Repl repl;

repl_start : {
  Context ctx;
  auto stmts = repl.Parse(&ctx.error_log_);
  if (ctx.num_errors() > 0) {
    ctx.DumpErrors();
    goto repl_start;
  }

  for (auto &stmt : stmts->content_) {
    if (stmt->is<AST::Declaration>()) {
      auto *decl = &stmt->as<AST::Declaration>();
      decl->assign_scope(ctx.mod_->global_.get());
      decl->Validate(&ctx);
      decl->EmitIR(&ctx);
      if (ctx.num_errors() != 0) {
        ctx.DumpErrors();
        goto repl_start;
      }

    } else if (stmt->is<AST::Expression>()) {
      auto *expr = &stmt->as<AST::Expression>();
      expr->assign_scope(ctx.mod_->global_.get());
      ReplEval(expr);
      fprintf(stderr, "\n");
    } else {
      NOT_YET(*stmt);
    }
  }
  goto repl_start;
}
}

int main(int argc, char *argv[]) {
#ifdef DBG
  signal(SIGABRT, debug::DumpStackTrace);
#endif

  RUN(timer, "Argument parsing") {
    switch (ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  return repl ? RunRepl() : GenerateCode();
}
