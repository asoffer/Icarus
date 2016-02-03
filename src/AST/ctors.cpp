#include "AST.h"

namespace AST {
  Expression::Expression() : expr_type_(Unknown) {}

  Identifier::Identifier(size_t line_num, const std::string& token_string) : alloc_(nullptr), is_function_arg_(false) {
    token_ = token_string;
    expr_type_ = Unknown;
    precedence_ =
      Language::precedence(Language::Operator::NotAnOperator);
    line_num_ = line_num;
  }


  FunctionLiteral::FunctionLiteral() :
    fn_scope_(new FnScope(nullptr)), llvm_function_(nullptr) {}

  TypeLiteral::TypeLiteral() :
    type_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}

  // TODO Will TypeScope suffice?
  EnumLiteral::EnumLiteral() :
    enum_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}

  While::While() : body_scope_(Scope::build<WhileScope>()) {}
}  // namespace AST
