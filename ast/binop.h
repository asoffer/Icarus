#ifndef ICARUS_AST_BINOP_H
#define ICARUS_AST_BINOP_H

#include <memory>
#include <vector>

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "frontend/operators.h"
#include "ir/val.h"

struct Scope;
struct Context;

namespace ast {
struct Binop : public Expression {
  ~Binop() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  std::vector<ir::Val> EmitIR(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
};

}  // namespace ast

#endif  // ICARUS_AST_BINOP_H
