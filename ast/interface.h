#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/literal.h"
#include "core/scope.h"

namespace ast {
struct Interface : public Literal {
  ~Interface() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(core::Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;

  ir::Results EmitIr(Context *ctx) override;

  std::vector<Declaration> decls_;
  std::unique_ptr<core::DeclScope> body_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_INTERFACE_H
