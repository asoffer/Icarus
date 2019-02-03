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

  std::string to_string(size_t n) const override { return token; }
  void assign_scope(Scope *scope) override;

  VerifyResult VerifyType(Context *) override;
  void Validate(Context *ctx) override;

  void ExtractJumps(JumpExprs *) const override {}

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::string token;
  Declaration *decl = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_IDENTIFIER_H
