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
TokenNode::TokenNode(TokenLocation loc, Language::NodeType in_node_type,
                     std::string str_lit)
    : Node(loc, in_node_type), tk_(std::move(str_lit)) {
  op = Language::is_operator(node_type()) ? Language::lookup_operator.at(tk_)
                                          : Language::Operator::NotAnOperator;
}

Expression::Expression()
    : precedence(Language::precedence(Language::Operator::NotAnOperator)),
      lvalue(false), type(Unknown) {}
Declaration::Declaration() : decl_type(DeclType::Std) {}
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

DummyTypeExpr::DummyTypeExpr(TokenLocation new_loc, Type *t) : type_value(t) {
  loc = new_loc;
}

Jump::Jump(TokenLocation new_loc, JumpType jump_type) : jump_type(jump_type) {
  loc = new_loc;
}

Identifier::Identifier() { assert(false); }

Identifier::Identifier(TokenLocation new_loc, const std::string &token_string)
    : alloc(nullptr), is_arg(false) {
  token_     = token_string;
  type       = Unknown;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  loc        = new_loc;
  type_      = Language::expr;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope), llvm_fn(nullptr), statements(nullptr),
      code_gened(false) {}

StructLiteral::StructLiteral() : type_value(nullptr), type_scope(new Scope) {}

EnumLiteral::EnumLiteral() : type_value(nullptr) {}

While::While() : while_scope(new BlockScope(ScopeType::While)) {}
For::For() : for_scope(new BlockScope(ScopeType::For)) {}
} // namespace AST
