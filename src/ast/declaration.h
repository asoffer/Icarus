#ifndef ICARUS_AST_DECLARATION_H
#define ICARUS_AST_DECLARATION_H

#include "ast/expression.h"
#include "ast/identifier.h"
#include "ir/register.h"

struct Module;
namespace IR {
struct Val;
}  // namespace IR

namespace AST {
struct Declaration : public Expression {
  Declaration(bool is_const = false) : const_(is_const) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;
  ~Declaration() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  Declaration *Clone() const override;
  void CloneTo(Declaration *) const;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override { UNREACHABLE(this); }

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<Expression> type_expr, init_val;

  // For non-const declarations, holds the address at which the value is being
  // stored. For const values (declared with :: or ::=), holds the actual
  // constant value.
  IR::Register addr_ = IR::Register{-1};

  bool const_ = false;

  // If it's an argument or return value, this points to the function for which
  // it's an argument. Otherwise this field is null.
  // TODO whether this is null is computable from the type and that might be all
  // we need.
  Expression *arg_val = nullptr;
  Module *mod_ = nullptr;

  // These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return !type_expr; }
  bool IsDefaultInitialized() const { return !init_val; }
  bool IsCustomInitialized() const;
};
}  // namespace AST

#endif  // ICARUS_AST_DECLARATION_H
