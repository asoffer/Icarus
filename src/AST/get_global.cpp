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
extern llvm::ConstantInt *const_true();
extern llvm::ConstantInt *const_false();
extern llvm::Constant *null(const Type *t);
} // namespace data

namespace builtin {
extern llvm::Function *ascii();
extern llvm::Function *ord();
} // namespace builtin

namespace AST {
llvm::Constant *Terminal::GetGlobal() {
  switch (terminal_type) {
  case Language::Terminal::ASCII: return builtin::ascii();
  case Language::Terminal::Char: return data::const_char(value.as_char);
  case Language::Terminal::Else: assert(false);
  case Language::Terminal::False: return data::const_false();
  case Language::Terminal::Hole: assert(false);
  case Language::Terminal::Int: return data::const_int(value.as_int);
  case Language::Terminal::Null: return data::null(type);
  case Language::Terminal::Ord: return builtin::ord();
  case Language::Terminal::Real: return data::const_real(value.as_real);
  case Language::Terminal::Return: assert(false);
  case Language::Terminal::StringLiteral: assert(false && "TODO");
  case Language::Terminal::True: return data::const_true();
  case Language::Terminal::Type: assert(false);
  case Language::Terminal::Uint: return data::const_uint(value.as_uint);
  }
}

llvm::Constant *ArrayLiteral::GetGlobal() {
  assert(type->is_array());
  auto array_type = (Array *)type;
  assert(array_type->fixed_length);

  std::vector<llvm::Constant *> constants(array_type->len, nullptr);

  for (size_t i = 0; i < array_type->len; ++i) {
    constants[i] = elems[i]->GetGlobal();
  }

  return llvm::ConstantArray::get(
      static_cast<llvm::ArrayType *>(array_type->llvm_type), constants);
}

llvm::Constant *Identifier::GetGlobal() {
  assert(alloc);
  return (llvm::Constant *)alloc;
}

llvm::Constant *Unop::GetGlobal() {
  switch (op) {
  case Language::Operator::And: {
    return operand->GetGlobal();
  } break;
  default: assert(false);
  }
}

#define NOT_YET(type)                                                          \
  llvm::Constant *type::GetGlobal() { assert(false && "Not yet implemented"); }

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
