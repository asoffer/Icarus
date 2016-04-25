#ifndef ICARUS_TYPE_OPS_H
#define ICARUS_TYPE_OPS_H

#include "Language.h"

extern llvm::Module *global_module;

llvm::Value *Primitive::call_add(llvm::Value *lhs, llvm::Value *rhs) {
  if (this == Int || this == Uint)  return builder.CreateAdd(lhs, rhs, "add");
  else if (this == Real)            return builder.CreateFAdd(lhs, rhs, "fadd");
  else                              return nullptr;
}

llvm::Value *Primitive::call_sub(llvm::Value *lhs, llvm::Value *rhs) {
  if (this == Int || this == Uint)  return builder.CreateSub(lhs, rhs, "sub");
  else if (this == Real)            return builder.CreateFSub(lhs, rhs, "fsub");
  else                              return nullptr;
}

llvm::Value *Primitive::call_mul(llvm::Value *lhs, llvm::Value *rhs) {
  if (this == Int || this == Uint)  return builder.CreateMul(lhs, rhs, "mul");
  else if (this == Real)            return builder.CreateFMul(lhs, rhs, "fmul");
  else                              return nullptr;
}

llvm::Value *Primitive::call_div(llvm::Value *lhs, llvm::Value *rhs) {
  if (this == Int)        return builder.CreateSDiv(lhs, rhs, "sdiv");
  else if (this == Uint)  return builder.CreateUDiv(lhs, rhs, "udiv");
  else if (this == Real)  return builder.CreateFDiv(lhs, rhs, "fdiv");
  else                    return nullptr;
}

llvm::Value *Primitive::call_mod(llvm::Value *lhs, llvm::Value *rhs) {
  if (this == Int)        return builder.CreateSRem(lhs, rhs, "smod");
  else if (this == Uint)  return builder.CreateURem(lhs, rhs, "umod");
  else if (this == Real)  return builder.CreateFRem(lhs, rhs, "fmod");
  else                    return nullptr;
}

llvm::Value* Primitive::call_neg(llvm::Value* operand) {
  if (this == Int)        return builder.CreateNeg(operand, "neg");
  else if (this == Real)  return builder.CreateFNeg(operand, "fneg");
  else                    return nullptr;
}

llvm::Value* Primitive::call_not(llvm::Value* operand) {
  return (this == Char ? builder.CreateNot(operand, "not") : nullptr);
}

llvm::Value *FunctionComposition(const std::string &name, llvm::Value *lhs,
                                 llvm::Value *rhs, Function *fn_type) {
  auto old_block = builder.GetInsertBlock();

  llvm::FunctionType *llvm_fn_type = *fn_type;

  auto llvm_fn = static_cast<llvm::Function *>(
      global_module->getOrInsertFunction(name, llvm_fn_type));

  auto entry = make_block("entry", llvm_fn);
  builder.SetInsertPoint(entry);

  // TODO multiple args, multiple return values, non-primitives, void return
  auto arg = llvm_fn->args().begin();
  builder.CreateRet(builder.CreateCall(lhs, {builder.CreateCall(rhs, {arg})}));

  builder.SetInsertPoint(old_block);
  return llvm_fn;
}

llvm::Value *Function::call_mul(llvm::Value *lhs, llvm::Value *rhs) {
  return FunctionComposition("__anon_fn", lhs, rhs, this);
}

#define BINARY_OPERATOR_MACRO(op, symbol, prec, assoc)                         \
  llvm::Value *TYPE::call_##op(llvm::Value *lhs, llvm::Value *rhs) {           \
    return nullptr;                                                            \
  }
#define LEFT_UNARY_OPERATOR_MACRO(op)                                          \
  llvm::Value *TYPE::call_##op(llvm::Value *operand) { return nullptr; }

#define TYPE Array 
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Tuple
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Pointer
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Function
#include "config/left_unary_operators.conf"
BINARY_OPERATOR_MACRO(add, +, 16, left)
BINARY_OPERATOR_MACRO(sub, -, 16, left)
BINARY_OPERATOR_MACRO(div, /, 17, left)
BINARY_OPERATOR_MACRO(mod, %, 17, left)
#undef TYPE

#define TYPE Structure
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Enumeration
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE DependentType
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE TypeVariable
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE ParametricStructure
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE QuantumType
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE RangeType
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#undef CHAIN_OPERATOR_MACRO
#undef BINARY_OPERATOR_MACRO

#endif // ICARUS_TYPE_OPS_H
