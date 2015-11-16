#include "AST.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;

namespace AST {
  llvm::Value* Identifier::generate_lvalue(Scope*) {
    return alloca_;
  } 

  // TODO
  llvm::Value* Unop::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* ChainOp::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* ArrayType::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Terminal::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* FunctionLiteral::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Case::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Assignment::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Declaration::generate_lvalue(Scope*) { return nullptr; }


  llvm::Value* Binop::generate_lvalue(Scope* scope) {
    if (lhs_->type()->is_array()) {
      auto lhs_val = lhs_->generate_lvalue(scope);
      auto rhs_val = rhs_->generate_code(scope);

      return builder.CreateGEP(lhs_->type()->llvm(),
          lhs_val, { llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(32, 0, true)), rhs_val }, "array_idx");
    }

    return nullptr;
  }
}  // namespace AST
