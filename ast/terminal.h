#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "ast/literal.h"
#include "ir/val.h"
#include "misc/module.h"

struct Context;

namespace ast {
struct Terminal : public Literal {
  Terminal() = default;
  Terminal(const TextSpan &span, ir::Val val)
      : Literal(span), value(std::move(val)) {}
  ~Terminal() override {}

  void assign_scope(Scope *scope) override { scope_ = scope; }
  std::string to_string(size_t) const override { return value.to_string(); }

  VerifyResult VerifyType(Context *ctx) override {
    return VerifyResult::Constant(ctx->set_type(this, value.type));
  }

  // TODO distinguish between guaranteed failures and failures to continue
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return value.type == type::Type_ &&
           std::get<type::Type const *>(value.value) == t;
  }

  void Validate(Context *) override {}
  void ExtractJumps(JumpExprs *) const override {}
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override {}

  std::vector<ir::Val> EmitIR(Context *) override { return {value}; }

  ir::Val value = ir::Val::None();
};
}  // namespace ast

#endif  // ICARUS_AST_TERMINAL_H
