#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "ast/fn_args.h"
#include "ast/literal.h"

namespace ast {
struct Call : public Literal {
  ~Call() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override;

  ir::Results EmitIr(Context *) override;

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
