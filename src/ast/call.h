#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "ast/fn_args.h"

namespace ast {
struct Call : public Expression {
  ~Call() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
