#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include "ast/dispatch.h"
#include "ast/expression.h"
#include "base/container/vector.h"
#include "frontend/operators.h"

namespace AST {
struct ChainOp : public Expression {
  ~ChainOp() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  ChainOp *Clone() const override;
  base::vector<Language::Operator> ops;
  base::vector<std::unique_ptr<Expression>> exprs;
  base::vector<DispatchTable> dispatch_tables_;
};
}  // namespace AST
#endif  // ICARUS_AST_CHAINOP_H
