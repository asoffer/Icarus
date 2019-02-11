#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/literal.h"
#include "misc/scope.h"

namespace ast {
struct Interface : public Literal {
  ~Interface() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::vector<Declaration> decls_;
  std::unique_ptr<DeclScope> body_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_INTERFACE_H
