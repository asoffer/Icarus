#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

namespace Language {
const std::map<std::string, Operator> lookup_operator = {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  { #symbol, Operator::name }                                                  \
  ,
#include "config/operator.conf"
#undef OPERATOR_MACRO
};
} // namespace Language

namespace AST {
TokenNode::TokenNode(TokenLocation loc, std::string str_lit)
    : Node(loc), token(str_lit) {
  auto iter = Language::lookup_operator.find(token);
  if (iter == Language::lookup_operator.end()) {
    op = Language::Operator::NotAnOperator;
  } else {
    op = iter->second;
  }
}

Expression::Expression()
    : precedence(Language::precedence(Language::Operator::NotAnOperator)),
      lvalue(false), type(nullptr), value(nullptr), value_flag(ValueFlag::Not) {
}
Declaration::Declaration()
    : identifier(nullptr), type_expr(nullptr), init_val(nullptr),
      alloc(nullptr), arg_val(nullptr) {}
Generic::Generic() : test_fn(nullptr) {}
InDecl::InDecl() : container(nullptr) {}
ArrayLiteral::ArrayLiteral() {}
Access::Access() {}
ChainOp::ChainOp() {}
Case::Case() {}
Binop::Binop() {}
Unop::Unop() {}
Terminal::Terminal() {}
ArrayType::ArrayType() {}
Eval::Eval() : expr(nullptr) {}

DummyTypeExpr::DummyTypeExpr() { assert(false); }

DummyTypeExpr::DummyTypeExpr(TokenLocation new_loc, Type *t) {
  loc = new_loc;
  value = Context::Value(t);
}

Jump::Jump(TokenLocation new_loc, JumpType jump_type) : jump_type(jump_type) {
  loc = new_loc;
}

Identifier::Identifier() { assert(false); }

Identifier::Identifier(TokenLocation new_loc, const std::string &token_string)
    : decl(nullptr) {
  token      = token_string;
  type       = nullptr;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  loc        = new_loc;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope), return_type_expr(nullptr), llvm_fn(nullptr),
      statements(nullptr), ir_func(nullptr), code_gened(false) {}

StructLiteral::StructLiteral() : type_scope(new Scope) {}
ParametricStructLiteral::ParametricStructLiteral() : type_scope(new Scope) {}

EnumLiteral::EnumLiteral() {}

While::While() : while_scope(new BlockScope(ScopeType::While)) {}
For::For() : for_scope(new BlockScope(ScopeType::For)) {}
} // namespace AST
