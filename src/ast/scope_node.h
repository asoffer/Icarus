#ifndef ICARUS_AST_SCOPE_NODE_H
#define ICARUS_AST_SCOPE_NODE_H

#include <memory>
#include "ast/expression.h"
#include "scope.h"

struct Context;

namespace AST {
struct ScopeNode : public Expression {
  ~ScopeNode() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  ScopeNode *Clone() const override;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  std::unique_ptr<Expression> expr, scope_expr;
  std::unique_ptr<Statements> stmts;
  std::unique_ptr<ExecScope> internal;
};
}  // namespace AST

#endif // ICARUS_AST_SCOPE_NODE_H
