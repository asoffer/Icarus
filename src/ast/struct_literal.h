#ifndef ICARUS_AST_STRUCT_LITERAL_H
#define ICARUS_AST_STRUCT_LITERAL_H

#include "ast/expression.h"
#include "scope.h"

namespace AST {
struct StructLiteral : public Expression {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

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
  StructLiteral *Clone() const override;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;

  std::unique_ptr<DeclScope> type_scope;
  std::vector<std::unique_ptr<Declaration>> fields_;
};
}  // namespace AST

#endif  // ICARUS_AST_STRUCT_LITERAL_H
