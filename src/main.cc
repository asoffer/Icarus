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

extern IR::Val Evaluate(AST::Expression *expr);

extern void VerifyDeclBeforeUsage();
extern void CompletelyVerify(AST::Node *node);
extern AST::Statements *Parse(Source *source);
extern std::vector<AST::Statements *>
ParseAllFiles(std::queue<std::string> file_names);
extern std::stack<Scope *> ScopeStack;
extern Timer timer;

// extern IR::Val GetInitialGlobal(size_t global_addr);
// extern void AddInitialGlobal(size_t global_addr, IR::Val initial_val);

AST::Statements *global_statements;

static u64 global_counter = 0;

void AST::Declaration::AllocateGlobal() {
  if (addr != IR::Val::None()) { return; }

  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

  if (type->has_vars() && init_val->is_function_literal()) {
    for (auto kv : ((AST::FunctionLiteral *)init_val)->cache) {
      kv.second->AllocateGlobal();
    }
    return;
  }

  addr = IR::Val::GlobalAddr(global_counter++, type);
}

void AST::Declaration::EmitGlobal() {
  verify_types();
  if (type == Err) { return; }

  if (addr == IR::Val::None() /*|| TODO
      GetInitialGlobal(addr.as_global_addr) != IR::Val::None() */) {
    return;
  }
  ASSERT(!arg_val, "");
  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

  if (type->is_pointer()) {
    NOT_YET;
    // addr = IR::Val::Error();
    // ErrorLog::GlobalPointerUnsupported(loc);
    // return;
  }

  if (!IsDefaultInitialized()) {
    ASSERT(init_val, "");
    if (type->has_vars() && init_val->is_function_literal()) {
      for (auto kv : ((AST::FunctionLiteral *)init_val)->cache) {
        kv.second->EmitGlobal();
      }
      return;
    } else {
      // auto eval_value = Evaluate(init_val);
      // if (eval_value == IR::Val::Error()) { return; }
      NOT_YET;
      // AddInitialGlobal(addr.as_global_addr, eval_value);
    }
  } else if (HasHashtag("cstdlib")) {
    auto cstr = new char[identifier->token.size() + 1];
    strcpy(cstr, identifier->token.c_str());
    NOT_YET;
    // AddInitialGlobal(addr.as_global_addr, IR::Val::ExtFn(cstr));
  } else {
    NOT_YET;
    // AddInitialGlobal(addr.as_global_addr, type->EmitInitialValue());
  }
}

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
    WITH_SCOPE(Scope::Global) { global_statements->assign_scope(); }
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

  while (true) {
    // Read
    bool continue_to_next_line;
    std::vector<std::string> input_lines;
    std::string line_feed = "> ";

    do {
      std::cout << line_feed;
      std::string input;
      std::getline(std::cin, input);
      continue_to_next_line = false;
      for (int i = static_cast<int>(input.size()); i >= 0; --i) {
        if (input[i] == ' ' || input[i] == '\t') { continue; }
        if (input[i] == '\\') {
          continue_to_next_line = i == 0 || input[i - 1] != '\\';
          break;
        }
      }

      input_lines.push_back(std::move(input));
      line_feed = "  ";
    } while (continue_to_next_line);

    ReplSource src(std::move(input_lines));
    auto stmts = Parse(&src);

    // Eval
    for (auto stmt : stmts->statements) {
      if (stmt->is_expression()) {
        std::cerr << Evaluate(static_cast<AST::Expression *>(stmt)).to_string()
                  << std::endl;
      }
    }

    // Print
    // TODO
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
