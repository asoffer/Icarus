#ifndef ICARUS_AST_SCOPE_LITERAL_H
#define ICARUS_AST_SCOPE_LITERAL_H

#include "ast/declaration.h"
#include "ast/expression.h"

namespace ast {
struct ScopeLiteral : public Expression {
  ScopeLiteral(bool stateful) : stateful_(stateful) {}
  ~ScopeLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  base::vector<Declaration> decls_;
  std::unique_ptr<Scope> body_scope_;
  bool stateful_ = false;
};
}  // namespace ast

#endif  // ICARUS_AST_SCOPE_LITERAL_H
