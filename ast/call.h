#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "ast/dispatch.h"
#include "ast/literal.h"
#include "ast/fn_args.h"

namespace ast {
struct Call : public Literal {
  ~Call() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
