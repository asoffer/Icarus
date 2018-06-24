#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/expression.h"
#include "scope.h"

namespace AST {
struct Interface : public Expression {
  ~Interface() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  std::vector<IR::Val> EmitIR(Context *) override;
  std::vector<IR::Val> EmitLVal(Context *) override;

  Interface *Clone() const override;

  std::vector<Declaration> decls_;
  std::unique_ptr<DeclScope> body_scope_;
};
}  // namespace AST

#endif  // ICARUS_AST_INTERFACE_H
