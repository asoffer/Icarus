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
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  ir::Results EmitIr(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override {
    UNREACHABLE(this);
  }

  std::string id_;
  std::unique_ptr<Expression> type_expr, init_val;

  Module *mod_ = nullptr;

  // Field in a function, whether or not it's an input our output.
  bool is_fn_param_ = false;
  bool is_output_   = false;
  bool const_       = false;

  // These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
  bool IsInferred() const { return !type_expr; }
  bool IsDefaultInitialized() const { return !init_val; }
  bool IsCustomInitialized() const;
};
}  // namespace ast

#endif  // ICARUS_AST_DECLARATION_H
