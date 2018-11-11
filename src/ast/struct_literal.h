#ifndef ICARUS_AST_STRUCT_LITERAL_H
#define ICARUS_AST_STRUCT_LITERAL_H

#include "ast/expression.h"
#include "scope.h"

namespace type {
struct Struct;
}  // namespace type

namespace ast {
struct StructLiteral : public Expression {
  StructLiteral()                          = default;
  StructLiteral(StructLiteral &&) noexcept = default;
  ~StructLiteral() override {}

  StructLiteral &operator=(StructLiteral &&) noexcept = default;

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  void Complete(type::Struct *s);

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;

  std::unique_ptr<DeclScope> type_scope;
  // TODO declartaions directly. don't need unique_ptr indirection.
  base::vector<std::unique_ptr<Declaration>> fields_;
  Module *mod_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_LITERAL_H
