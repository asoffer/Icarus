#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "ast/expression.h"
#include "ir/val.h"
#include "module.h"

struct Context;

namespace ast {
struct Terminal : public Expression {
  Terminal() = default;
  Terminal(const TextSpan &span, ir::Val val);

  ~Terminal() override {}

  void assign_scope(Scope *scope) override;
  std::string to_string(size_t) const override { return value.to_string(); }

  type::Type const *VerifyType(Context *ctx) override;

  void Validate(Context *) override {}
  void ExtractJumps(JumpExprs *) const override {}

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *ct) override;

  ir::Val value = ir::Val::None();
};
}  // namespace ast

#endif  // ICARUS_AST_TERMINAL_H
