#ifndef ICARUS_AST_SCOPE_LITERAL_H
#define ICARUS_AST_SCOPE_LITERAL_H

#include "ast/declaration.h"
#include "ast/expression.h"

namespace AST {
struct ScopeLiteral : public Expression {
  ~ScopeLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  ScopeLiteral *Clone() const override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  base::vector<Declaration> decls_;
  std::unique_ptr<Scope> body_scope_;
};
}  // namespace AST

#endif  // ICARUS_AST_SCOPE_LITERAL_H
