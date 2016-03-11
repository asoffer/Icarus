#include "AST.h"

namespace AST {
Expression::Expression() : type(Unknown) {}
Declaration::Declaration() : decl_type(DeclType::Std) {}
ArrayLiteral::ArrayLiteral() {}
Access::Access() {}
ChainOp::ChainOp() {}
Case::Case() {}
Binop::Binop() {}
Unop::Unop() {}
Terminal::Terminal() {}
ArrayType::ArrayType() {}

Identifier::Identifier(size_t input_line_num, const std::string &token_string)
    : alloc(nullptr), is_function_arg(false), decl(nullptr) {
  token_ = token_string;
  type = Unknown;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  line_num = input_line_num;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope(nullptr)), llvm_fn(nullptr) {}

TypeLiteral::TypeLiteral() : type_value(nullptr), type_scope(new TypeScope) {}

EnumLiteral::EnumLiteral() : type_value(nullptr) {}

While::While() : while_scope(new Scope) {}
For::For() : for_scope(new Scope) {}
} // namespace AST
