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

std::vector<IR::Func *> implicit_functions;

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

extern void Parse(SourceFile *sf);
extern std::stack<Scope *> ScopeStack;

static Timer timer;

extern llvm::Module *global_module;
extern llvm::TargetMachine *target_machine;

extern void GenerateLLVMTypes();

namespace IR {
extern std::vector<IR::Value> InitialGlobals;
extern std::vector<llvm::GlobalVariable *> LLVMGlobals;
} // namespace IR

std::queue<AST::Node *> VerificationQueue;
std::queue<std::pair<Type *, AST::Statements *>> FuncInnardsVerificationQueue;
std::map<std::string, SourceFile *> source_map;

#include "tools.h"

void AST::Declaration::EmitGlobal() {
  assert(!arg_val);
  verify_types();

  if (type->is_pointer()) {
    Error::Log::Log(loc, "We do not support global pointers yet.");
    return;
  }

  if (type->is_array()) {
    Error::Log::Log(loc, "We do not support global arrays yet.");
    return;
  }

  if (!IsDefaultInitialized()) {
    assert(init_val);
    if (type->has_vars() && init_val->is_function_literal()) {
      for (auto kv : ((AST::FunctionLiteral *)init_val)->cache) {
        kv.second->EmitGlobal();
      }
      return;
    } else {
      addr                                    = IR::Value::CreateGlobal();
      IR::InitialGlobals[addr.as_global_addr] = Evaluate(init_val);
    }
  } else {
    addr                                    = IR::Value::CreateGlobal();
    IR::InitialGlobals[addr.as_global_addr] = type->EmitInitialValue();
  }

  if (file_type != FileType::None) {
    // TODO What if we're returning a struct we haven't created fully yet?
    auto type_for_llvm =
        type->is_function() && HasHashtag("const") ? type : Ptr(type);
    type_for_llvm->generate_llvm();

    IR::LLVMGlobals[addr.as_global_addr] = new llvm::GlobalVariable(
        /*      Module = */ *global_module,
        /*        Type = */ *type_for_llvm,
        /*  isConstant = */ HasHashtag("const"),
        /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
        /* Initializer = */ nullptr,
        /*        Name = */ identifier->token);
  }
}

void AST::Declaration::EmitLLVMGlobal() {
  if (file_type == FileType::None) { return; }
  assert(!arg_val);
  verify_types();

  if (type->is_struct() || type->is_parametric_struct() || type->is_range() ||
      type->is_slice() || type->time() == Time::compile) {
    return;
  }

  if (type->has_vars()) {
    if (init_val && init_val->is_function_literal()) {
      for (auto kv : ((AST::FunctionLiteral *)init_val)->cache) {
        kv.second->EmitLLVMGlobal();
      }
    } else {
      return;
    }
  }

  assert(type->is_primitive() || type->is_pointer() || type->is_enum() ||
         type->is_function());

  if (type->is_function() && HasHashtag("const")) {
    if (init_val) {
      if (init_val->is_function_literal()) {
        auto func = init_val->EmitIR().as_func;
        func->GenerateLLVM();
        func->llvm_fn->setName(
            Mangle((Function *)type, identifier, Scope::Global));
        return;
      } else {
        NOT_YET;
      }
    } else {
      NOT_YET;
    }
  }

  auto ir_val = IR::InitialGlobals[addr.as_global_addr];
  llvm::Constant *llvm_val;
  switch (ir_val.flag) {
  case IR::ValType::B: llvm_val   = data::const_bool(ir_val.as_bool); break;
  case IR::ValType::C: llvm_val   = data::const_char(ir_val.as_char); break;
  case IR::ValType::I: llvm_val   = data::const_int(ir_val.as_int); break;
  case IR::ValType::R: llvm_val   = data::const_real(ir_val.as_real); break;
  case IR::ValType::U16: llvm_val = data::const_uint16(ir_val.as_uint16); break;
  case IR::ValType::U32: llvm_val = data::const_uint32(ir_val.as_uint32); break;
  case IR::ValType::U: llvm_val   = data::const_uint(ir_val.as_uint); break;
  case IR::ValType::F:
    ir_val.as_func->dump();
    ir_val.as_func->GenerateLLVM();
    assert(ir_val.as_func->llvm_fn);
    llvm_val = ir_val.as_func->llvm_fn;
    break;
  case IR::ValType::GlobalAddr:
    llvm_val = IR::LLVMGlobals[ir_val.as_global_addr];
    break;
  default: std::cerr << ir_val << std::endl; NOT_YET;
  }
  IR::LLVMGlobals[addr.as_global_addr]->setInitializer(llvm_val);

  return;
}

int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch(ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  if (debug::ct_eval) { initscr(); }
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

  RUN(timer, "Verify and Emit") {
    for (auto stmt : global_statements->statements) {
      if (stmt->is_declaration()) {
        ((AST::Declaration *)stmt)->EmitGlobal();

      } else if (stmt->is_unop()) {
        switch (((AST::Unop *)stmt)->op) {
        case Language::Operator::Eval: 
          if (((AST::Unop *)stmt)->type == Void) {
            Evaluate(((AST::Unop *)stmt)->operand);
          } else {
            Error::Log::GlobalNonDecl(stmt->loc);
          }
          case Language::Operator::Import: break;
          default: Error::Log::GlobalNonDecl(stmt->loc); break;
        }

      } else {
        Error::Log::GlobalNonDecl(stmt->loc);
      }
    }
  }

  // Is there anything left here TODO?
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
      auto stmts = pair.second;
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

  CHECK_FOR_ERRORS; // NOTE: The only reason we check for errors here is because
                    // we want to stop due to finding a global array or pointer.
                    // Once we allow global arrays, we don't need this check
                    // anymore.

  if (file_type != FileType::None) {
    RUN(timer, "Code-gen") {
      for (auto decl : Scope::Global->DeclRegistry) { decl->EmitLLVMGlobal(); }

      // Generate all the functions
      if (file_type != FileType::None) {
        for (auto f : implicit_functions) {
          if (f->generated == IR::Func::Gen::ToLink) { continue; }
          f->GenerateLLVM();
        }
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
