#ifndef ICARUS_AST_DECLARATION_H
#define ICARUS_AST_DECLARATION_H

#include "ast/expression.h"
#include "ir/register.h"

struct Module;
namespace ir {
struct Val;
}  // namespace ir

namespace ast {
struct Declaration : public Expression {
  Declaration(bool is_const = false) : const_(is_const) {}
  Declaration(Declaration &&) noexcept = default;
  Declaration &operator=(Declaration &&) noexcept = default;
  ~Declaration() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override { UNREACHABLE(this); }

  std::string id_;
  std::unique_ptr<Expression> type_expr, init_val;

  // If it's an argument or return value, this points to the function for which
  // it's an argument. Otherwise this field is null.
  Module *mod_ = nullptr;
  bool is_arg_ = false;
  bool const_  = false;

  // These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return !type_expr; }
  bool IsDefaultInitialized() const { return !init_val; }
  bool IsCustomInitialized() const;
};
}  // namespace ast

#endif  // ICARUS_AST_DECLARATION_H
