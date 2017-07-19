#include "ast.h"

#include "../scope.h"
#include "../type/type.h"

namespace AST {
TokenNode::TokenNode(const Cursor &loc, std::string str)
    : Node(loc), token(std::move(str)) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  if (token == symbol) {                                                       \
    op = Language::Operator::name;                                             \
    return;                                                                    \
  }
#include "config/operator.conf"
#undef OPERATOR_MACRO
  op = Language::Operator::NotAnOperator;
}

Expression::Expression()
    : precedence(Language::precedence(Language::Operator::NotAnOperator)),
      lvalue(Assign::Unset), type(nullptr), value(IR::Val::None()) {}

Terminal::Terminal(const Cursor &cursor, IR::Val val) {
  type          = val.type;
  precedence    = Language::precedence(Language::Operator::NotAnOperator);
  loc           = cursor;
  value         = val;
}

ScopeLiteral::ScopeLiteral(const Cursor &cursor) {
  loc   = cursor;
  value = IR::Val::None(); // TODO Scope(this);
}

Jump::Jump(const Cursor &new_loc, JumpType jump_type) : jump_type(jump_type) {
  loc = new_loc;
}

CommaList::CommaList() {
  precedence = Language::precedence(Language::Operator::Comma);
}

Identifier::Identifier(const Cursor &new_loc, const std::string &token_string) {
  token      = token_string;
  type       = nullptr;
  precedence = Language::precedence(Language::Operator::NotAnOperator);
  loc        = new_loc;
}
} // namespace AST
