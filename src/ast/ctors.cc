#include "ast.h"

#include "../type/type.h"

namespace Language {
extern size_t precedence(Operator op);
} // namespace Language

namespace AST {
TokenNode::TokenNode(const TextSpan &span, std::string str)
    : Node(span), token(std::move(str)) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  if (token == symbol) {                                                       \
    op = Language::Operator::name;                                             \
    return;                                                                    \
  }
#include "config/operator.conf"
#undef OPERATOR_MACRO
  op = Language::Operator::NotAnOperator;
}

Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  type          = val.type;
  precedence    = Language::precedence(Language::Operator::NotAnOperator);
  value         = std::move(val);
}

Jump::Jump(const TextSpan &span, JumpType jump_type)
    : Node(span), jump_type(jump_type) {}

CommaList::CommaList() {
  precedence = Language::precedence(Language::Operator::Comma);
}

Identifier::Identifier(const TextSpan &id_span,
                       const std::string &token_string) {
  span       = id_span;
  token      = token_string;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
}
} // namespace AST
