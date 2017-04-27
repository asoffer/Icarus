#include <cstring>
#include <ncurses.h>
#include <vector>

#include "ast/ast.h"
#include "base/file.h"
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
extern void Parse(File *sf);
extern void ParseAllFiles();
extern std::stack<Scope *> ScopeStack;
extern Timer timer;

// extern IR::Val GetInitialGlobal(size_t global_addr);
// extern void AddInitialGlobal(size_t global_addr, IR::Val initial_val);

std::map<std::string, File *> source_map;
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

/*
struct NCursesScopeGuard{
  NCursesScopeGuard() {
    if (debug::ct_eval) { initscr(); }
  }

  ~NCursesScopeGuard() {
    if (debug::ct_eval) { endwin(); }
  }
};
*/
int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch (ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  // NCursesScopeGuard ncurses_scope_guard;

  ParseAllFiles();
  CHECK_FOR_ERRORS;

  RUN(timer, "AST Setup") {
    size_t num_stmts = 0;
    for (const auto &kv : source_map) { num_stmts += kv.second->ast->size(); }

    global_statements = new AST::Statements;
    global_statements->statements.reserve(num_stmts);
    for (auto &kv : source_map) { global_statements->add(kv.second->ast); }

    ScopeStack.push(Scope::Global);
    global_statements->assign_scope();
    ScopeStack.pop();
  }

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

  return 0;
}
