#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "ast/expression.h"
#include "ir/val.h"
#include "module.h"

struct Context;

namespace AST {
struct Terminal : public Expression {
  Terminal() = default;
  Terminal(const TextSpan &span, IR::Val val);

  ~Terminal() override {}

  void assign_scope(Scope *scope) override;
  std::string to_string(size_t) const override { return value.to_string(); }

  type::Type const *VerifyType(Context *ctx) override;

  void Validate(Context *) override {}
  void ExtractJumps(JumpExprs *) const override {}

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *ct) override;

  IR::Val value = IR::Val::None();
};
}  // namespace AST

#endif  // ICARUS_AST_TERMINAL_H
