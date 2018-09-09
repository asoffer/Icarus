#ifndef ICARUS_AST_BINOP_H
#define ICARUS_AST_BINOP_H

#include <memory>
#include "base/container/vector.h"

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "frontend/operators.h"
#include "ir/val.h"

struct Scope;
struct Context;

namespace AST {
struct Binop : public Expression {
  ~Binop() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  Binop *Clone() const override;
  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
  DispatchTable dispatch_table_;
};

}  // namespace AST

#endif  // ICARUS_AST_BINOP_H
