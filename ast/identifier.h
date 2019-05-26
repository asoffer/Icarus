#ifndef ICARUS_AST_IDENTIFIER_H
#define ICARUS_AST_IDENTIFIER_H

#include "ast/expression.h"

namespace ast {
struct Declaration;

struct Identifier : public Expression {
  Identifier() {}  // TODO needed?
  Identifier(const TextSpan &span, std::string token)
      : Expression(span), token(std::move(token)) {}
  ~Identifier() override {}

#include "visitor/visitors.xmacro.h"

  std::string token;
  // TODO determine if mutability here is safe. It's thread-hostile, but maybe
  // you can make the rules clear enough. Or maybe store it elsewhere, but that
  // seems like overkill. This should only be set by VerifyType.
  mutable Declaration const *decl_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_IDENTIFIER_H
