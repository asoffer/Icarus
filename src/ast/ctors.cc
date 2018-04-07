#include "ast.h"
#include "stages.h"

namespace AST {
TokenNode::TokenNode(const TextSpan &span, std::string str)
    : Node(span), token(std::move(str)) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  if (token == symbol) {                                                       \
    op = Language::Operator::name;                                             \
    return;                                                                    \
  }
#include "../frontend/operators.xmacro.h"
#undef OPERATOR_MACRO
  op = Language::Operator::NotAnOperator;
}

Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  stage_range_.low = Validated;
  type   = val.type;
  lvalue = Assign::Const;
  value  = std::move(val);
}

Jump::Jump(const TextSpan &span, JumpType jump_type)
    : Node(span), jump_type(jump_type) {}

Identifier::Identifier(const TextSpan &id_span,
                       const std::string &token_string) {
  span  = id_span;
  token = token_string;
}

CodeBlock::CodeBlock() {
  lvalue = Assign::Const;
  type   = type::Code;
}

CodeBlock::CodeBlock(std::string s) : content_(std::move(s)) {
  lvalue = Assign::RVal;
  type   = type::Code;
}

GenericFunctionLiteral::GenericFunctionLiteral() {
  lvalue = Assign::Const;
  type   = type::Generic;
}
}  // namespace AST
