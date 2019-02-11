#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/literal.h"

namespace ast {
// TODO consider separating this into two classes given that we know when we
// parse if it has parens or not.
struct Switch : public Literal {
  ~Switch() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *rets) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::unique_ptr<Expression> expr_;
  std::vector<
      std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>
      cases_;
};
}  // namespace ast

#endif  // ICARUS_AST_SWITCH_H
