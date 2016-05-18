#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

#ifdef DEBUG
#define AT(access) .at((access))
#else
#define AT(access) [(access)]
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
      lvalue(false), type(nullptr), value(nullptr) {}
Declaration::Declaration()
    : expr(nullptr), decl_type(DeclType::Std) {}
InDecl::InDecl() {}
ArrayLiteral::ArrayLiteral() {}
Access::Access() {}
ChainOp::ChainOp() {}
Case::Case() {}
Binop::Binop() {}
Unop::Unop() {}
Terminal::Terminal() {}
ArrayType::ArrayType() {}

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
    : alloc(nullptr), is_arg(false) {
  token      = token_string;
  type       = nullptr;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  loc        = new_loc;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope), llvm_fn(nullptr), statements(nullptr),
      code_gened(false) {}

StructLiteral::StructLiteral() : type_scope(new Scope) {}
ParametricStructLiteral::ParametricStructLiteral() : type_scope(new Scope) {}

EnumLiteral::EnumLiteral() {}

While::While() : while_scope(new BlockScope(ScopeType::While)) {}
For::For() : for_scope(new BlockScope(ScopeType::For)) {}
} // namespace AST

#undef DEBUG
