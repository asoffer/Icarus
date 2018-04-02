#ifndef ICARUS_AST_UNOP_H
#define ICARUS_AST_UNOP_H

#include "expression.h"

namespace AST {
struct Unop : public Expression {
  EXPR_FNS(Unop);
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Unop *Clone() const override;
  std::unique_ptr<Expression> operand;
  Language::Operator op;
  DispatchTable dispatch_table_;
};
} //
#endif  // ICARUS_AST_UNOP_H
