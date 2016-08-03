#ifndef ICARUS_UNITY
#include "SourceFile.h"
#include "Type/Type.h"
#include "Scope.h"
#include "IR/IR.h"
#include "util/command_line_args.h"
#include "util/timer.h"
#include <ncurses.h>
#endif

#define CHECK_FOR_ERRORS                                                       \
  do {                                                                         \
    if (Error::Log::NumErrors() != 0) {                                        \
      Error::Log::Dump();                                                      \
      if (debug::ct_eval) { endwin(); }                                        \
      return -1;                                                               \
    }                                                                          \
  } while (false)

std::vector<IR::Func *> all_functions;

extern IR::Value Evaluate(AST::Expression *expr);

namespace data {
extern llvm::Constant *null(const Type *t);
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::ConstantInt *const_uint16(uint16_t n);
extern llvm::ConstantInt *const_uint32(uint32_t n);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantFP *const_real(double d);
} // namespace data

extern std::vector<IR::Func *> all_functions;
extern void Parse(SourceFile *sf);
extern std::stack<Scope *> ScopeStack;

static Timer timer;

extern llvm::Module *global_module;
extern llvm::TargetMachine *target_machine;

extern void GenerateLLVMTypes();

namespace IR {
extern std::vector<IR::Value> InitialGlobals;
extern std::vector<llvm::Constant *> LLVMGlobals;
} // namespace IR

std::queue<AST::Node *> VerificationQueue;
std::queue<std::pair<Type *, AST::Statements *>> FuncInnardsVerificationQueue;
std::map<std::string, SourceFile *> source_map;

#include "tools.h"

int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch(ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  RUN(timer, "Icarus Initialization") {
    if (debug::ct_eval) { initscr(); }
  }

  if (file_type != FileType::None) { InitializeLLVM(); }

  while (!file_queue.empty()) {
    std::string file_name = file_queue.front();
    file_queue.pop();
    if (source_map.find(file_name) != source_map.end()) { continue; }

    RUN(timer, "Parsing a file") {
      auto source_file      = new SourceFile(file_name);
      source_map[file_name] = source_file;
      Parse(source_file);
    }
  }

  CHECK_FOR_ERRORS; // Lexing and parsing

  AST::Statements *global_statements;

  RUN(timer, "AST Setup") {
    // Combine all statement nodes from separately-parsed files.
    global_statements = new AST::Statements;

    // Reserve enough space for all of them to avoid unneeded copies
    size_t num_statements = 0;
    for (const auto &kv : source_map) {
      num_statements += kv.second->ast->size();
    }
    global_statements->reserve(num_statements);

    for (auto &kv : source_map) {
      global_statements->add_nodes(kv.second->ast);
    }

    // COMPILATION STEP:
    //
    // Determine which declarations go in which scopes. Store that information
    // with the scopes. Note that assign_scope cannot possibly generate
    // compilation errors, so we don't check for them here.
    ScopeStack.push(Scope::Global);
    global_statements->assign_scope();
    ScopeStack.pop();
  }

  // COMPILATION STEP:
  //
  // For each identifier, figure out which other identifiers are needed in
  // order to declare this one. This cannot generate compilation errors.
  // Dependency::record(global_statements);
  // To assign type orders, we traverse the dependency graph looking for a
  // valid ordering in which we can determine the types of the nodes. This can
  // generate compilation errors if no valid ordering exists.
  // Dependency::assign_order();
  RUN(timer, "Type verification") {
    VerificationQueue.push(global_statements);
    while (!VerificationQueue.empty()) {
      auto node_to_verify = VerificationQueue.front();
      node_to_verify->verify_types();
      VerificationQueue.pop();
    }

    while (!FuncInnardsVerificationQueue.empty()) {
      auto pair     = FuncInnardsVerificationQueue.front();
      auto ret_type = pair.first;
      auto stmts    = pair.second;
      stmts->VerifyReturnTypes(ret_type);
      FuncInnardsVerificationQueue.pop();
    }

    CHECK_FOR_ERRORS;

    if (file_type != FileType::None) { GenerateLLVMTypes(); }
  }

  RUN(timer, "(L/R)value checking") {
    global_statements->lrvalue_check();
    CHECK_FOR_ERRORS;
  }

  RUN(timer, "Top-level verification") {
    for (auto stmt : global_statements->statements) {
      if (stmt->is_declaration() ||
          (stmt->is_unop() &&
           ((((AST::Unop *)stmt)->op == Language::Operator::Eval &&
             ((AST::Unop *)stmt)->type == Void) ||
            ((AST::Unop *)stmt)->op == Language::Operator::Import))) {
        continue;
      }
      Error::Log::GlobalNonDecl(stmt->loc);
    }

    CHECK_FOR_ERRORS;
  }

  RUN(timer, "Emit-IR") {
    for (auto decl : Scope::Global->DeclRegistry) {
      if (decl->arg_val || decl->type->time() == Time::compile ||
          decl->type->is_function()) {
        continue;
      }

      if (decl->type->is_pointer()) {
        Error::Log::Log(decl->loc, "We do not support global pointers yet.");
        continue;
      }


      if (decl->type->is_array()) {
        Error::Log::Log(decl->loc, "We do not support global arrays yet.");
        continue;
      }

      decl->addr = IR::Value::CreateGlobal();
      if (decl->IsInferred() || decl->IsCustomInitialized()) {
        assert(decl->init_val);
        IR::InitialGlobals[decl->addr.as_global_addr] =
            Evaluate(decl->init_val);
      } else if (decl->IsDefaultInitialized()) {
        IR::InitialGlobals[decl->addr.as_global_addr] =
            decl->type->EmitInitialValue();
      } else {
        NOT_YET;
      }
    }

    for (auto decl : Scope::Global->DeclRegistry) {
      if (decl->arg_val || !decl->type->is_function() ||
          decl->type->has_vars()) {
        continue;
      }
      decl->identifier->EmitIR();
    }
  }

  CHECK_FOR_ERRORS; // NOTE: The only reason we check for errors here is because
                    // we want to stop due to finding a global array or pointer.
                    // Once we allow global arrays, we don't need this check
                    // anymore.

  if (file_type != FileType::None) {
    RUN(timer, "Code-gen") {
      // Globals
      for (auto decl : Scope::Global->DeclRegistry) {
        assert(!decl->arg_val);
        auto id   = decl->identifier;
        auto type = decl->type;

        if (type->is_struct() || type->is_parametric_struct() ||
            type->is_range() || type->is_slice() ||
            type->time() == Time::compile) {
          continue;
        }

        if (type->is_function()) { continue; /* TODO what if it's #mutable */ }

        if (type->is_primitive() || type->is_pointer() || type->is_enum()) {
          auto gvar = new llvm::GlobalVariable(
              /*      Module = */ *global_module,
              /*        Type = */ *type,
              /*  isConstant = */ decl->HasHashtag("const"),
              /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
              /* Initializer = */ 0, // might be specified below
              /*        Name = */ id->token);

          auto ir_val = IR::InitialGlobals[decl->addr.as_global_addr];
          llvm::Constant *init_val;
          switch (ir_val.flag) {
          case IR::ValType::B:
            init_val = data::const_bool(ir_val.as_bool);
            break;
          case IR::ValType::C:
            init_val = data::const_char(ir_val.as_char);
            break;
          case IR::ValType::I: init_val = data::const_int(ir_val.as_int); break;
          case IR::ValType::R:
            init_val = data::const_real(ir_val.as_real);
            break;
          case IR::ValType::U16:
            init_val = data::const_uint16(ir_val.as_uint16);
            break;
          case IR::ValType::U32:
            init_val = data::const_uint32(ir_val.as_uint32);
            break;
          case IR::ValType::U:
            init_val = data::const_uint(ir_val.as_uint);
            break;
          case IR::ValType::GlobalAddr:
            init_val = IR::LLVMGlobals[ir_val.as_global_addr];
            break;
          default: NOT_YET;
          }

          gvar->setInitializer(init_val);
          IR::LLVMGlobals[decl->addr.as_global_addr] = gvar;

          continue;
        }

        std::cerr << *type << std::endl;
        UNREACHABLE;
      }

      // Generate all the functions
      if (file_type != FileType::None) {
        for (auto f : all_functions) {
          if (f->generated == IR::Func::Gen::ToLink) { continue; }
          f->GenerateLLVM();
        }
      }

      { // Generate code for everything else
        ScopeStack.push(Scope::Global);
        for (auto stmt : global_statements->statements) {
          if (stmt->is_declaration()) { continue; }
          if (stmt->is_unop()) {
            if (((AST::Unop *)stmt)->op == Language::Operator::Eval) {
              Evaluate(((AST::Unop *)stmt)->operand);
            }
            continue;
          }
          UNREACHABLE;
        }
        ScopeStack.pop();
      }
    }
  }

  switch (file_type) {
  case FileType::IR: {
    RUN(timer, "Writing IR") {
      std::ofstream output_file_stream(output_file_name);
      llvm::raw_os_ostream output_file(output_file_stream);
      global_module->print(output_file, nullptr);
    }
  } break;

  case FileType::Nat: {
    WriteObjectFile(output_file_name);
  } break;

  case FileType::Bin: {
    WriteObjectFile("obj.o");

    RUN(timer, "Link/system") {
      int buff_size  = (int)(strlen(output_file_name) + 24);
      char *link_str = new char[buff_size];
      int num_bytes_written =
          sprintf(link_str, "gcc obj.o -o %s; rm obj.o", output_file_name);
      assert(buff_size = num_bytes_written + 1);
      system(link_str);
      delete[] link_str;
    }
  } break;

  case FileType::None: /* Do nothing, just exit */ break;
  }

  if (debug::ct_eval) { endwin(); }
  return 0;
}
