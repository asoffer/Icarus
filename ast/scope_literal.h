#ifndef ICARUS_AST_SCOPE_LITERAL_H
#define ICARUS_AST_SCOPE_LITERAL_H

#include "ast/declaration.h"
#include "ast/literal.h"

namespace ast {
struct ScopeLiteral : public Literal {
  ScopeLiteral(bool stateful) : stateful_(stateful) {}
  ~ScopeLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::vector<Declaration> decls_;
  std::unique_ptr<Scope> body_scope_;
  bool stateful_ = false;
};
}  // namespace ast

#endif  // ICARUS_AST_SCOPE_LITERAL_H
