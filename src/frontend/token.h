#ifndef ICARUS_FRONTEND_TOKEN_H
#define ICARUS_FRONTEND_TOKEN_H

#include <string>
#include "ast/node.h"
#include "frontend/operators.h"

namespace frontend {
// AST node used only for holding tokens which have been lexed but not yet
// parsed.
struct Token : public AST::Node {
  Token(const TextSpan &span = TextSpan(), std::string str = "")
      : Node(span), token(std::move(str)) {
#define OPERATOR_MACRO(name, symbol, prec, assoc)                              \
  if (token == symbol) {                                                       \
    op = Language::Operator::name;                                             \
    return;                                                                    \
  }
#include "operators.xmacro.h"
#undef OPERATOR_MACRO
    op = Language::Operator::NotAnOperator;
  }

  ~Token() override {}

  std::string to_string(size_t) const override { return "[" + token + "]\n"; }

  void assign_scope(Scope *) override { UNREACHABLE(); }
  Token *Clone() const override { UNREACHABLE(); }
  type::Type const *VerifyType(Context *) override { UNREACHABLE(); }
  void Validate(Context *) override { UNREACHABLE(); }
  void ExtractJumps(AST::JumpExprs *) const override { UNREACHABLE(); }

  void SaveReferences(Scope *, base::vector<IR::Val> *) override {
    UNREACHABLE();
  }
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const AST::Expression *, IR::Val> &) override {
    UNREACHABLE();
  }
  base::vector<IR::Val> EmitIR(Context *) override { UNREACHABLE(); }

  std::string token;
  Language::Operator op;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_TOKEN_H
