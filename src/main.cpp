#ifndef ICARUS_UNITY
#include "SourceFile.h"
#include "Type/Type.h"
#include "Scope.h"
#include "IR/IR.h"
#include "util/command_line_args.h"
#include <ncurses.h>
#endif

#define CHECK_FOR_ERRORS                                                       \
  do {                                                                         \
    if (ErrorLog::NumErrors() != 0) {                                          \
      ErrorLog::Dump();                                                        \
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

extern void VerifyDeclBeforeUsage();
extern void CompletelyVerify(AST::Node *node);
extern void Parse(SourceFile *sf);
extern void ParseAllFiles();
extern std::stack<Scope *> ScopeStack;
extern Timer timer;
extern llvm::Module *global_module;
extern llvm::TargetMachine *target_machine;
extern void GenerateLLVMTypes();

extern IR::Value GetInitialGlobal(size_t global_addr);
extern void AddInitialGlobal(size_t global_addr, IR::Value initial_val);

namespace IR {
extern std::vector<llvm::Constant *> LLVMGlobals;
} // namespace IR

llvm::Value *main_fn = nullptr;
extern llvm::IRBuilder<> builder;
extern llvm::BasicBlock *make_block(const std::string &name,
                                    llvm::Function *fn);

std::map<std::string, SourceFile *> source_map;
AST::Statements *global_statements;

#include "tools.h"

void AST::Declaration::AllocateGlobal() {
  if (addr != IR::Value::None()) { return; }

  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

  if (type->has_vars() && init_val->is_function_literal()) {
    for (auto kv : ((AST::FunctionLiteral *)init_val)->cache) {
      kv.second->AllocateGlobal();
    }
    return;
  }

  addr = IR::Value::CreateGlobal();

  if (file_type != FileType::None) {
    if (type->time() == Time::compile) { return; }

    auto ptype = Ptr(type);
    ptype->generate_llvm();

    if (HasHashtag("cstdlib")) {
      // TODO assuming a function type
      llvm::FunctionType *ft = *(Function *)type;
      IR::LLVMGlobals[addr.as_global_addr] = new llvm::GlobalVariable(
          /*      Module = */ *global_module,
          /*        Type = */ *(type->is_function() ? ptype : type),
          /*  isConstant = */ true,
          /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
          /* Initializer = */ global_module->getOrInsertFunction(
              identifier->token, ft),
          /*        Name = */ identifier->token);
    } else {
      IR::LLVMGlobals[addr.as_global_addr] = new llvm::GlobalVariable(
          /*      Module = */ *global_module,
          /*        Type = */ *(type->is_function() ? ptype : type),
          /*  isConstant = */ false, // TODO HasHashtag("const"),
          /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
          /* Initializer = */ nullptr,
          /*        Name = */ type->is_function()
              ? Mangle((Function *)type, identifier, Scope::Global)
              : identifier->token);
      // TODO fix mangler to take any types not just functions
    }
  }
}

void AST::Declaration::EmitGlobal() {
  verify_types();
  if (type == Err) { return; }
 
  if (GetInitialGlobal(addr.as_global_addr) != IR::Value::None()) { return; }
  assert(!arg_val);
  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

  if (type->is_pointer()) {
    addr = IR::Value::Error();
    ErrorLog::GlobalPointerUnsupported(loc);
    return;
  }

  if (type->is_array()) {
    AddInitialGlobal(addr.as_global_addr, IR::Value::Error());
    ErrorLog::GlobalArrayUnsupported(loc);
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
      auto eval_value = Evaluate(init_val);
      if (eval_value == IR::Value::Error()) { return; }
      AddInitialGlobal(addr.as_global_addr, eval_value);
    }
  } else if (HasHashtag("cstdlib")) {
    auto cstr = new char[identifier->token.size() + 1];
    strcpy(cstr, identifier->token.c_str());
    AddInitialGlobal(addr.as_global_addr, IR::Value::ExtFn(cstr));
  } else {
    AddInitialGlobal(addr.as_global_addr, type->EmitInitialValue());
  }

  if (file_type != FileType::None) {
    if (identifier->token == "main") {
      main_fn = IR::LLVMGlobals[addr.as_global_addr];
    }
  }
}

void AST::Declaration::EmitLLVMGlobal() {
  if (file_type == FileType::None) { return; }
  assert(!arg_val);
  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

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

  if (HasHashtag("cstdlib")) { return; }

  if (type->is_function() && HasHashtag("const")) {
    if (init_val) {
      if (init_val->is_function_literal()) {
        auto func = init_val->EmitIR().as_func;
        func->GenerateLLVM();
        // TODO do we need this here?
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

  auto ir_val = GetInitialGlobal(addr.as_global_addr);
  llvm::Constant *llvm_val = nullptr;
  switch (ir_val.flag) {
  case IR::ValType::B: llvm_val   = data::const_bool(ir_val.as_bool); break;
  case IR::ValType::C: llvm_val   = data::const_char(ir_val.as_char); break;
  case IR::ValType::I: llvm_val   = data::const_int(ir_val.as_int); break;
  case IR::ValType::R: llvm_val   = data::const_real(ir_val.as_real); break;
  case IR::ValType::U16: llvm_val = data::const_uint16(ir_val.as_uint16); break;
  case IR::ValType::U32: llvm_val = data::const_uint32(ir_val.as_uint32); break;
  case IR::ValType::U: llvm_val   = data::const_uint(ir_val.as_uint); break;
  case IR::ValType::F:
    ir_val.as_func->GenerateLLVM();
    assert(ir_val.as_func->llvm_fn);
    llvm_val = ir_val.as_func->llvm_fn;
    break;
  case IR::ValType::GlobalAddr:
    llvm_val = IR::LLVMGlobals[ir_val.as_global_addr];
    break;
  case IR::ValType::ExtFn: global_module->getFunction(ir_val.as_ext_fn); break;
  default: std::cerr << ir_val << std::endl; NOT_YET;
  }

  ((llvm::GlobalVariable *)IR::LLVMGlobals[addr.as_global_addr])
      ->setInitializer(llvm_val);

  return;
}

int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch (ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  if (debug::ct_eval) { initscr(); }
  if (file_type != FileType::None) { InitializeLLVM(); }

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
        case Language::Operator::Import: break;
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

  if (file_type != FileType::None) {
    RUN(timer, "Generate LLVM types") { GenerateLLVMTypes(); }
  }

  // TODO needs to be earlier/ part of type verification
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
      llvm::FunctionType *ft = *Func(Void, Void);
      // TODO this looks fishy
      llvm::Function *fn =
          (llvm::Function *)global_module->getOrInsertFunction("main", ft);
      auto block = make_block("entry", fn);

      assert(main_fn);
      builder.SetInsertPoint(block);
      builder.CreateCall(builder.CreateLoad(main_fn), {});
      builder.CreateRetVoid();
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
