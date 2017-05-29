#include <cstring>
#include <ncurses.h>
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

std::vector<IR::Func *> implicit_functions;

extern void ReplEval(AST::Expression *expr);

extern void VerifyDeclBeforeUsage();
extern std::vector<AST::Statements *>
ParseAllFiles(std::queue<std::string> file_names);
extern Timer timer;

AST::Statements *global_statements;

namespace debug {
extern bool timer;
extern bool parser;
extern bool ct_eval;
} // namespace debug

int GenerateCode() {
  std::vector<AST::Statements *> stmts_by_file =
      ParseAllFiles(std::move(file_queue));

  CHECK_FOR_ERRORS;

  RUN(timer, "AST Setup") {
    global_statements = AST::Statements::Merge(std::move(stmts_by_file));
    global_statements->assign_scope(Scope::Global);
  }

  RUN(timer, "Verify and Emit") {
    for (auto stmt : global_statements->statements) {
      std::cerr << stmt->to_string(0) << std::endl;
    }
  }

  /*
    RUN(timer, "Verify and Emit") {
      for (auto stmt : global_statements->statements) {
        if (!stmt->is_declaration()) { continue; }
        ((AST::Declaration *)stmt)->AllocateGlobal();
      }

      for (auto stmt : global_statements->statements) {
        if (stmt->is_declaration()) {
          ((AST::Declaration *)stmt)->EmitGlobal();

        } else if (stmt->is_unop()) {
          switch (((AST::Unop *)stmt)->op) {
          case Language::Operator::Eval:
            stmt->verify_types();
            if (ErrorLog::num_errs_ > 0) { continue; }

            if (((AST::Unop *)stmt)->type == Void) {
              Evaluate(((AST::Unop *)stmt)->operand);
            } else {
              ErrorLog::GlobalNonDecl(stmt->loc);
            }
          case Language::Operator::Require: break;
          default: ErrorLog::GlobalNonDecl(stmt->loc); break;
          }

        } else {
          ErrorLog::GlobalNonDecl(stmt->loc);
        }
      }
    }

    RUN(timer, "Type verification") {
      CompletelyVerify(global_statements);
      VerifyDeclBeforeUsage();
      CHECK_FOR_ERRORS;
    }

    // TODO needs to be earlier/ part of type verification
    RUN(timer, "(L/R)value checking") {
      global_statements->lrvalue_check();
      CHECK_FOR_ERRORS;
    }

    if (file_type == FileType::None) { return 0; }

   //  RUN(timer, "Code-gen") {
   //    // Generate all the functions
   //    if (file_type != FileType::None) {
   //      for (auto f : implicit_functions) {
   //        if (f->generated == IR::Func::Gen::ToLink) { continue; }
   //      }
   //    }
   //  }

    switch (file_type) {
    case FileType::None: UNREACHABLE;
    case FileType::Nat: UNREACHABLE;
    case FileType::IR: UNREACHABLE;
    case FileType::Bin: UNREACHABLE;
    }
  */
  return 0;
}

int RunRepl() {
  std::cout << "Icarus REPL (v0.1)" << std::endl;

  Repl repl;
  while (true) {
    auto stmts = repl.Parse();
    for (auto stmt : stmts->statements) {
      if (stmt->is_declaration()) {
        auto decl = static_cast<AST::Declaration *>(stmt);
        decl->assign_scope(Scope::Global);
        std::vector<Error> errors;
        decl->EmitIR(&errors);

      } else if (stmt->is_expression()) {
        auto expr = static_cast<AST::Expression *>(stmt);
        expr->assign_scope(Scope::Global);
        ReplEval(expr);
        std::cerr << std::endl;
      } else {
        NOT_YET;
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
