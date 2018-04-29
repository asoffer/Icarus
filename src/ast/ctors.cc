#include "ast.h"
#include "stages.h"

namespace type {
extern Type *Generic;
}  // namespace type

namespace AST {
Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  stage_range_.low = DoneBodyValidationStage;
  type   = val.type;
  lvalue = Assign::Const;
  value  = std::move(val);
}

Identifier::Identifier(const TextSpan &id_span,
                       const std::string &token_string) {
  span  = id_span;
  token = token_string;
}

GenericFunctionLiteral::GenericFunctionLiteral() {
  lvalue = Assign::Const;
  type   = type::Generic;
}
}  // namespace AST
