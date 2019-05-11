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

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override { return token; }

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
  // TODO determine if mutability here is safe. It's thread-hostile, but maybe
  // you can make the rules clear enough. Or maybe store it elsewhere, but that
  // seems like overkill. This should only be set by VerifyType.
  mutable Declaration const *decl_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_IDENTIFIER_H
