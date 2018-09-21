#ifndef ICARUS_AST_STRUCT_LITERAL_H
#define ICARUS_AST_STRUCT_LITERAL_H

#include "ast/expression.h"
#include "scope.h"

namespace type {
struct Struct;
}  // namespace type

namespace AST {
struct StructLiteral : public Expression {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;
  StructLiteral *Clone() const override;

  void Complete(type::Struct *s);

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  std::unique_ptr<DeclScope> type_scope;
  base::vector<std::unique_ptr<Declaration>> fields_;
  Module *mod_ = nullptr;
};
}  // namespace AST

#endif  // ICARUS_AST_STRUCT_LITERAL_H
