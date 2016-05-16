#ifndef ICARUS_UNITY
#include "Scope.h"
#include "Type.h"
#endif

namespace data {
extern llvm::ConstantInt *const_bool(bool b);
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantFP *const_real(double d);
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

namespace AST {
llvm::Constant *Terminal::GetGlobal(Context &ctx) {
  if (type == Bool) {
    return data::const_bool(terminal_type == Language::Terminal::True);

  } else if (type == Char) {
    return data::const_char(token()[0]);

  } else if (type == Int) {
    return data::const_int(std::stol(token()));

  } else if (type == Real) {
    return data::const_real(std::stod(token()));

  } else if (type == Uint) {
    return data::const_uint(std::stoul(token()));
  }

  assert(false);
}

llvm::Constant *ArrayLiteral::GetGlobal(Context &ctx) { 
  assert(type->is_array());
  auto array_type = (Array *)type;
  assert(array_type->fixed_length);

  std::vector<llvm::Constant *> constants(array_type->len, nullptr);

  for (size_t i = 0; i < array_type->len; ++i) {
    constants[i] = elems[i]->GetGlobal(ctx);
  }

  return llvm::ConstantArray::get(
      static_cast<llvm::ArrayType *>(array_type->llvm_type), constants);
}

llvm::Constant *Identifier::GetGlobal(Context &ctx) {
  assert(alloc);
  return (llvm::Constant *)alloc;
}

llvm::Constant *Unop::GetGlobal(Context &ctx) {
  switch (op) {
  case Language::Operator::And: {
    return operand->GetGlobal(ctx);
  } break;
  default: assert(false);
  }
}

#define NOT_YET(type)                                                          \
  llvm::Constant *type::GetGlobal(Context &ctx) {                              \
    assert(false && "Not yet implemented");                                    \
  }

NOT_YET(InDecl)
NOT_YET(Declaration)
NOT_YET(Binop)
NOT_YET(ChainOp)
NOT_YET(Case)
NOT_YET(DummyTypeExpr)
NOT_YET(Access)
NOT_YET(ArrayType)
NOT_YET(EnumLiteral)
NOT_YET(StructLiteral)
NOT_YET(ParametricStructLiteral)
NOT_YET(FunctionLiteral)

} // namespace AST

#undef NOT_YET
