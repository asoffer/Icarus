#include <future>
#include <cstring>
#include <vector>

#include "ast/ast.h"
#include "ast/statements.h"
#include "backend/emit.h"
#include "base/debug.h"
#include "base/source.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "module.h"
#include "util/command_line_args.h"
#include "util/timer.h"

Timer timer;

struct Scope;
extern Scope *GlobalScope;

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

extern void ReplEval(AST::Expression *expr);

void ScheduleParse(const Source::Name &src);
extern Timer timer;
extern std::unordered_map<Source::Name, std::future<AST::Statements>> modules;

AST::Statements global_statements;

int GenerateCode() {
  for (const auto &src : files) {
    ScheduleParse(src);
  }

  // TODO This is hacky and probably racy.
  size_t current_size = modules.size();
  do {
    current_size = modules.size();
    for (auto & [ src, module ] : modules) { module.wait(); }
  } while (current_size != modules.size());

  std::vector<AST::Statements> stmts_by_file;
  for (auto & [ src, module ] : modules) {
    stmts_by_file.push_back(std::move(module.get()));
  }

  RUN(timer, "AST Setup") {
    global_statements = AST::Statements::Merge(std::move(stmts_by_file));
  }

  Context ctx;
  RUN(timer, "Verify and Emit") {
    AST::DoStages<0, 1>(&global_statements, GlobalScope, &ctx);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return 0;
    }
    AST::DoStage<2>(&global_statements, GlobalScope, &ctx);
    if (ctx.num_errors() != 0) {
      ctx.DumpErrors();
      return 0;
    }
    }

    RUN(timer, "Verify preconditions") {
      std::queue<IR::Func *> validation_queue;
      for (const auto &fn : ctx.mod_.fns_) { validation_queue.push(fn.get()); }

      int num_errors = 0;
      while (!validation_queue.empty()) {
        auto fn = std::move(validation_queue.front());
        validation_queue.pop();
        num_errors += fn->ValidateCalls(&validation_queue);
      }
    }

    RUN(timer, "LLVM") {
      backend::EmitAll(ctx.mod_.fns_, ctx.mod_.llvm_.get());
    }

    // Tag main
    for (const auto &stmt : global_statements.content_) {
      if (!stmt->is<AST::Declaration>()) { continue; }
      auto &decl = stmt->as<AST::Declaration>();
      if (decl.identifier->token != "main") { continue; }
      auto val = Evaluate(decl.init_val.get(), &ctx);
      ASSERT_EQ(val.size(), 1u);
      auto fn_lit = std::get<AST::FunctionLiteral *>(val[0].value);
      // TODO check more than one?

      fn_lit->ir_func_->llvm_fn_->setName("main");
      fn_lit->ir_func_->llvm_fn_->setLinkage(
          llvm::GlobalValue::ExternalLinkage);
    }

    ctx.mod_.llvm_->dump();
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
      AST::DoStages<0, 2>(decl, GlobalScope, &ctx);
      if (ctx.num_errors() != 0) {
        ctx.DumpErrors();
        goto repl_start;
      }

    } else if (stmt->is<AST::Expression>()) {
      auto *expr = &stmt->as<AST::Expression>();
      expr->assign_scope(GlobalScope);
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
#ifdef DEBUG
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
