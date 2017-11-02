#include <cstring>
#include <vector>

#include "ast/ast.h"
#include "base/source.h"
#include "error_log.h"
#include "ir/ir.h"
#include "scope.h"
#include "type/type.h"
#include "util/command_line_args.h"
#include "util/timer.h"

#define CHECK_FOR_ERRORS                                                       \
  do {                                                                         \
    if (ErrorLog::NumErrors() != 0) {                                          \
      ErrorLog::Dump();                                                        \
      return -1;                                                               \
    }                                                                          \
  } while (false)

extern void ReplEval(AST::Expression *expr);

extern void VerifyDeclBeforeUsage();
extern std::vector<AST::Statements *> ParseAllFiles();
extern Timer timer;

base::owned_ptr<AST::Statements> global_statements;

namespace debug {
extern bool timer;
extern bool parser;
extern bool ct_eval;
} // namespace debug

int GenerateCode() {
  auto stmts_by_file = ParseAllFiles();

  CHECK_FOR_ERRORS;

  RUN(timer, "AST Setup") {
    global_statements = AST::Statements::Merge(std::move(stmts_by_file));
    global_statements->assign_scope(Scope::Global);
  }

  RUN(timer, "Verify and Emit") {
    for (auto& stmt : global_statements->statements) {
      if (!stmt->is<AST::Declaration>()) { continue; }
      auto *decl = ptr_cast<AST::Declaration>(stmt.get());
      decl->EmitIR();
    }
  }

  RUN(timer, "Verify preconditions") {
    std::queue<IR::Func *> validation_queue;
    for (const auto &fn : IR::Func::All) { validation_queue.push(fn.get()); }

    int num_errors = 0;
    while (!validation_queue.empty()) {
      auto fn = std::move(validation_queue.front());
      validation_queue.pop();
      num_errors += fn->ValidateCalls(&validation_queue);
    }
  }

  return 0;
}

int RunRepl() {
  std::cout << "Icarus REPL (v0.1)" << std::endl;

  Repl repl;
  while (true) {
    auto stmts = repl.Parse();
    for (auto& stmt : stmts->statements) {
      if (stmt->is<AST::Declaration>()) {
        auto* decl = ptr_cast<AST::Declaration>(stmt.get());
        decl->assign_scope(Scope::Global);
        decl->EmitIR();

      } else if (stmt->is<AST::Expression>()) {
        auto* expr = ptr_cast<AST::Expression>(stmt.get());
        expr->assign_scope(Scope::Global);
        ReplEval(expr);
        std::cerr << std::endl;
      } else {
        NOT_YET(*stmt);
      }
    }
  }
  return 0;
}

int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch (ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  return repl ? RunRepl() : GenerateCode();
}
