#ifndef ICARUS_UNITY
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
      return -1;                                                               \
    }                                                                          \
  } while (false)

std::vector<IR::Func *> implicit_functions;

extern IR::Value Evaluate(AST::Expression *expr);

extern void VerifyDeclBeforeUsage();
extern void CompletelyVerify(AST::Node *node);
extern void Parse(SourceFile *sf);
extern void ParseAllFiles();
extern std::stack<Scope *> ScopeStack;
extern Timer timer;
extern llvm::Module *global_module;
extern llvm::TargetMachine *target_machine;

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

    if (HasHashtag("cstdlib")) {
      // TODO assuming a function type
      llvm::FunctionType *ft = *(Function *)type;
      IR::LLVMGlobals[addr.as_loc->GetGlobalAddr()] = new llvm::GlobalVariable(
          /*      Module = */ *global_module,
          /*        Type = */ *(type->is_function() ? Ptr(type) : type),
          /*  isConstant = */ true,
          /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
          /* Initializer = */ global_module->getOrInsertFunction(
              identifier->token, ft),
          /*        Name = */ identifier->token);
    } else {
      IR::LLVMGlobals[addr.as_loc->GetGlobalAddr()] = new llvm::GlobalVariable(
          /*      Module = */ *global_module,
          /*        Type = */ *(type->is_function() ? Ptr(type) : type),
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

  if (addr == IR::Value::None() ||
      GetInitialGlobal(addr.as_loc->GetGlobalAddr()) != IR::Value::None()) {
    return;
  }
  assert(!arg_val);
  verify_types();
  if (ErrorLog::num_errs_ > 0) { return; }

  if (type->is_pointer()) {
    addr = IR::Value::Error();
    ErrorLog::GlobalPointerUnsupported(loc);
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
      AddInitialGlobal(addr.as_loc->GetGlobalAddr(), eval_value);
    }
  } else if (HasHashtag("cstdlib")) {
    auto cstr = new char[identifier->token.size() + 1];
    strcpy(cstr, identifier->token.c_str());
    AddInitialGlobal(addr.as_loc->GetGlobalAddr(), IR::Value::ExtFn(cstr));
  } else {
    AddInitialGlobal(addr.as_loc->GetGlobalAddr(), type->EmitInitialValue());
  }

  if (file_type != FileType::None) {
    if (identifier->token == "main") {
      main_fn = IR::LLVMGlobals[addr.as_loc->GetGlobalAddr()];
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
        auto func = init_val->EmitIR().as_val->GetFunc();
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

  auto ir_val = GetInitialGlobal(addr.as_loc->GetGlobalAddr());
  llvm::Constant *llvm_val = nullptr;
  switch (ir_val.flag) {
  case IR::ValType::Val: llvm_val = ir_val.as_val->llvm(); break;
  case IR::ValType::Loc:
    if (ir_val.as_loc->is_global_addr()) {
      llvm_val = IR::LLVMGlobals[ir_val.as_loc->GetGlobalAddr()];
    } else {
      std::cerr << ir_val << std::endl;
      NOT_YET;
    }
    break;
  case IR::ValType::ExtFn: global_module->getFunction(ir_val.as_ext_fn); break;
  default: std::cerr << ir_val << std::endl; NOT_YET;
  }

  ((llvm::GlobalVariable *)IR::LLVMGlobals[addr.as_loc->GetGlobalAddr()])
      ->setInitializer(llvm_val);

  return;
}

struct NCursesScopeGuard{
  NCursesScopeGuard() {
    if (debug::ct_eval) { initscr(); }
  }

  ~NCursesScopeGuard() {
    if (debug::ct_eval) { endwin(); }
  }
};

int main(int argc, char *argv[]) {
  RUN(timer, "Argument parsing") {
    switch (ParseCLArguments(argc, argv)) {
      case CLArgFlag::QuitSuccessfully: return 0;
      case CLArgFlag::QuitWithFailure: return -1;
      case CLArgFlag::Continue:;
    }
  }

  NCursesScopeGuard ncurses_scope_guard;

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

  switch (file_type) {
  case FileType::None: UNREACHABLE;
  case FileType::Nat: WriteObjectFile(output_file_name); break;

  case FileType::IR: {
    RUN(timer, "Writing IR") {
      std::ofstream output_file_stream(output_file_name);
      llvm::raw_os_ostream output_file(output_file_stream);
      global_module->print(output_file, nullptr);
    }
  } break;


  case FileType::Bin: {
    WriteObjectFile("obj.o");

    RUN(timer, "Link/system") {
      std::string link_str =
          "gcc obj.o -o " + std::string(output_file_name) + "; rm obj.o";
      system(link_str.c_str());
    }
  } break;
  }

  return 0;
}
