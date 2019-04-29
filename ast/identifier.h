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
  void assign_scope(core::Scope *scope) override;

  VerifyResult VerifyType(Context *) override;

  void ExtractJumps(JumpExprs *) const override {}
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override {
    g->ids_[token].push_back(d);
  }
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::string token;
  Declaration *decl_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_IDENTIFIER_H
