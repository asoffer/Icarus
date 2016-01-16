#include "AST.h"

extern llvm::Module* global_module;
extern llvm::IRBuilder<> builder;

namespace data {
  extern llvm::Value* const_uint(size_t n);
}  // namespace data

namespace AST {
  llvm::Value* Identifier::generate_lvalue(Scope* scope) {
    return alloc_;
  } 

  // TODO
  llvm::Value* Unop::generate_lvalue(Scope*)            { return nullptr; }
  llvm::Value* ChainOp::generate_lvalue(Scope*)         { return nullptr; }
  llvm::Value* ArrayType::generate_lvalue(Scope*)       { return nullptr; }
  llvm::Value* ArrayLiteral::generate_lvalue(Scope*)    { return nullptr; }
  llvm::Value* Terminal::generate_lvalue(Scope*)        { return nullptr; }
  llvm::Value* FunctionLiteral::generate_lvalue(Scope*) { return nullptr; }
  llvm::Value* Case::generate_lvalue(Scope*)            { return nullptr; }
  llvm::Value* Assignment::generate_lvalue(Scope*)      { return nullptr; }
  llvm::Value* Declaration::generate_lvalue(Scope*)     { return nullptr; }
  llvm::Value* TypeLiteral::generate_lvalue(Scope*)     { return nullptr; }
  llvm::Value* EnumLiteral::generate_lvalue(Scope*)     { return nullptr; }

  llvm::Value* Binop::generate_lvalue(Scope* scope) {
    if (op_ == Language::Operator::Index && lhs_->type()->is_array()) {
      auto lhs_val = lhs_->generate_lvalue(scope);
      auto rhs_val = rhs_->generate_code(scope);
      auto load_ptr = scope->builder().CreateLoad(lhs_val);
      return scope->builder().CreateGEP(type()->llvm(), load_ptr, { rhs_val }, "array_idx");

    } else if (op_ == Language::Operator::Access && lhs_->type()->is_user_defined()) {
      auto lhs_type = static_cast<UserDefined*>(lhs_->type());
      auto lhs_lval = lhs_->generate_lvalue(scope);

      // TODO TOKENREMOVAL
      // rhs must needs to be an identifier in this case. use this to access token()
      return scope->builder().CreateGEP(lhs_type->llvm(), lhs_lval,
          { data::const_uint(0), lhs_type->field_num(rhs_->token()) });
    }

    return nullptr;
  }

}  // namespace AST
