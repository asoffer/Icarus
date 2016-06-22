#ifndef ICARUS_UNITY
#include "Type/Type.h"
#endif

extern llvm::Module *global_module;
extern llvm::IRBuilder<> builder;

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

namespace AST {
llvm::Value *Identifier::generate_lvalue() { 
  assert(decl);
  return decl->alloc;
}

llvm::Value *Unop::generate_lvalue() {
  if (op == Language::Operator::At) { return operand->generate_code(); }

  return nullptr;
}

// TODO these should all be assert(false)
llvm::Value *ChainOp::generate_lvalue() { return nullptr; }
llvm::Value *ArrayType::generate_lvalue() { return nullptr; }
llvm::Value *ArrayLiteral::generate_lvalue() { return nullptr; }
llvm::Value *Terminal::generate_lvalue() { return nullptr; }
llvm::Value *FunctionLiteral::generate_lvalue() { return nullptr; }
llvm::Value *Case::generate_lvalue() { return nullptr; }
llvm::Value *Generic::generate_lvalue() { return nullptr; }
llvm::Value *InDecl::generate_lvalue() { return nullptr; }
llvm::Value *Declaration::generate_lvalue() { return nullptr; }
llvm::Value *StructLiteral::generate_lvalue() { return nullptr; }
llvm::Value *ParametricStructLiteral::generate_lvalue() { return nullptr; }
llvm::Value *EnumLiteral::generate_lvalue() { return nullptr; }
llvm::Value *Eval::generate_lvalue() { return nullptr; }
llvm::Value *DummyTypeExpr::generate_lvalue() { return nullptr; }

llvm::Value *Access::generate_lvalue() {
  // Automatically pass through pointers
  auto etype  = operand->type;
  auto e_lval = operand->generate_lvalue();

  while (etype->is_pointer()) {
    etype  = ((Pointer *)etype)->pointee;
    e_lval = builder.CreateLoad(e_lval);
  }

  auto struct_type = (Structure *)etype;
  return builder.CreateGEP(
      e_lval, {data::const_uint(0), struct_type->field_num(member_name)});
}

llvm::Value *Binop::generate_lvalue() {
  if (op == Language::Operator::Index && lhs->type->is_array()) {
    auto array_type = (Array*)lhs->type;
    auto lhs_val  = lhs->generate_lvalue();
    auto rhs_val  = rhs->generate_code();
    if (array_type->fixed_length) {
      return builder.CreateGEP(lhs_val, {data::const_uint(0), rhs_val});
    } else {
        auto data_ptr = builder.CreateLoad(builder.CreateGEP(
            lhs_val, {data::const_uint(0), data::const_uint(1)}));
      return builder.CreateGEP(data_ptr, rhs_val, "array_idx");
    }
  }
  return nullptr;
}

} // namespace AST
