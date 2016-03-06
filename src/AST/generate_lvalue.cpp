#include "AST.h"

extern llvm::Module *global_module;

namespace data {
extern llvm::Value *const_uint(size_t n);
} // namespace data

namespace AST {
llvm::Value *Identifier::generate_lvalue() { return alloc; }

llvm::Value *Unop::generate_lvalue() {
  if (op == Language::Operator::At) { return operand->generate_code(); }

  return nullptr;
}

llvm::Value *ChainOp::generate_lvalue() { return nullptr; }
llvm::Value *ArrayType::generate_lvalue() { return nullptr; }
llvm::Value *ArrayLiteral::generate_lvalue() { return nullptr; }
llvm::Value *Terminal::generate_lvalue() { return nullptr; }
llvm::Value *FunctionLiteral::generate_lvalue() { return nullptr; }
llvm::Value *Case::generate_lvalue() { return nullptr; }
llvm::Value *Assignment::generate_lvalue() { return nullptr; }
llvm::Value *Declaration::generate_lvalue() { return nullptr; }
llvm::Value *TypeLiteral::generate_lvalue() { return nullptr; }
llvm::Value *EnumLiteral::generate_lvalue() { return nullptr; }

llvm::Value *Access::generate_lvalue() {
  // Automatically pass through pointers
  auto etype  = operand->type;
  auto e_lval = operand->generate_lvalue();

  while (etype->is_pointer()) {
    etype  = static_cast<Pointer *>(etype)->pointee;
    e_lval = CurrentBuilder().CreateLoad(e_lval);
  }

  auto struct_type = static_cast<Structure *>(etype);
  return CurrentBuilder().CreateGEP(
      e_lval, {data::const_uint(0), struct_type->field_num(member_name)});
}

llvm::Value *Binop::generate_lvalue() {
  if (op == Language::Operator::Index && lhs->type->is_array()) {
    auto lhs_val  = lhs->generate_lvalue();
    auto rhs_val  = rhs->generate_code();
    auto data_ptr = CurrentBuilder().CreateLoad(CurrentBuilder().CreateGEP(
        lhs_val, {data::const_uint(0), data::const_uint(1)}));
    return CurrentBuilder().CreateGEP(data_ptr, {rhs_val}, "array_idx");
  }
  return nullptr;
}

} // namespace AST
