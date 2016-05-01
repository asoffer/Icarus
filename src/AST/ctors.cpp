#include "Scope.h"

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
TokenNode::TokenNode(size_t line_num, Language::NodeType in_node_type,
                     std::string str_lit)
    : Node(line_num, in_node_type), tk_(std::move(str_lit)) {
  op = Language::is_operator(node_type()) ? Language::lookup_operator.at(tk_)
                                          : Language::Operator::NotAnOperator;
}

Expression::Expression() : lvalue(false), type(Unknown) {}
Declaration::Declaration() : decl_type(DeclType::Std) {}
ArrayLiteral::ArrayLiteral() {}
Access::Access() {}
ChainOp::ChainOp() {}
Case::Case() {}
Binop::Binop() {}
Unop::Unop() {}
Terminal::Terminal() {}
ArrayType::ArrayType() {}

DummyTypeExpr::DummyTypeExpr() { assert(false); }

DummyTypeExpr::DummyTypeExpr(size_t expr_line_num, Type *t) : type_value(t) {
  line_num   = expr_line_num;
}

Identifier::Identifier() { assert(false); }

Identifier::Identifier(size_t input_line_num, const std::string &token_string)
    : alloc(nullptr), is_arg(false) {
  token_     = token_string;
  type       = Unknown;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  line_num   = input_line_num;
}

FunctionLiteral::FunctionLiteral()
    : fn_scope(new FnScope), llvm_fn(nullptr), code_gened(false) {}

StructLiteral::StructLiteral() : type_value(nullptr), type_scope(new Scope) {}

EnumLiteral::EnumLiteral() : type_value(nullptr) {}

While::While() : while_scope(new BlockScope(ScopeType::While)) {}
For::For() : for_scope(new BlockScope(ScopeType::For)) {}
} // namespace AST
