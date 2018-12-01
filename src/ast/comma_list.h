#ifndef ICARUS_AST_COMMA_LIST_H
#define ICARUS_AST_COMMA_LIST_H

#include "ast/expression.h"

namespace ast {
struct CommaList : public Expression {
  CommaList()                      = default;
  CommaList(CommaList &&) noexcept = default;
  ~CommaList() override {}

  CommaList &operator=(CommaList &&) noexcept = default;

  void assign_scope(Scope *scope) override;
  std::string to_string(size_t n) const override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;

  base::vector<std::unique_ptr<Expression>> exprs_;
  // Only to be used during. Essentially, the parser builds up a comma-list
  // element by element, so when it sees <comma-list> <comma> <expression>, it
  // needs to know if the expression should be appended to the comma-list or it
  // should be a new two-element comma-list whose first element is a comma-list.
  // That is, it needs to distinguish between (1, 2, 3) and ((1, 2), 3). Because
  // we shed parentheses at parse-time, this boolean flag allows us to determine
  // the difference.
  bool closed_ = false;
};
}  // namespace ast

#endif  // ICARUS_AST_COMMA_LIST_H
