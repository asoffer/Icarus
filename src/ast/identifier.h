#ifndef ICARUS_AST_IDENTIFIER_H
#define ICARUS_AST_IDENTIFIER_H

#include "ast/expression.h"

namespace AST {
struct Declaration;

struct Identifier : public Expression {
  Identifier() {}  // TODO needed?
  Identifier(const TextSpan &span, const std::string &token)
      : Expression(span), token(token) {}
  ~Identifier() override {}

  std::string to_string(size_t n) const override { return token; }
  void assign_scope(Scope *scope) override;

  type::Type const *VerifyType(Context *) override;
  void Validate(Context *ctx) override;

  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override {}
  void ExtractReturns(base::vector<const Expression *> *) const override {}
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override {}

  Identifier *Clone() const override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  std::string token;
  Declaration *decl = nullptr;
};
}  // namespace AST

#endif  // ICARUS_AST_IDENTIFIER_H
