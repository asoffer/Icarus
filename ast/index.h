#ifndef ICARUS_AST_INDEX_H
#define ICARUS_AST_INDEX_H

#include <memory>
#include <vector>

#include "ast/expression.h"

struct Scope;
struct Context;

namespace ast {
struct Index : public Expression {
  ~Index() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  std::vector<ir::Val> EmitIR(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::unique_ptr<Expression> lhs_, rhs_;
};

}  // namespace ast

#endif  // ICARUS_AST_INDEX_H
