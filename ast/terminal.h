#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "ast/literal.h"
#include "ir/val.h"
#include "misc/module.h"
#include "misc/context.h"

struct Context;

namespace ast {
struct Terminal : public Literal {
  Terminal() = default;
  Terminal(const TextSpan &span, ir::Results results, type::Type const *t)
      : Literal(span), results_(std::move(results)), type_(t) {}
  ~Terminal() override {}

  void assign_scope(core::Scope *scope) override { scope_ = scope; }
  std::string to_string(size_t) const override {
    return "<<terminal: " + type_->to_string() + ">>";
  }

  VerifyResult VerifyType(Context *ctx) override {
    return VerifyResult::Constant(ctx->set_type(this, type_));
  }

  // TODO distinguish between guaranteed failures and failures to continue
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return type_ == type::Type_ &&
           results_.get<type::Type const *>(0).val_ == t;
  }

  void ExtractJumps(JumpExprs *) const override {}
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override {}

  ir::Results EmitIr(Context *ctx) override { return results_; };

  ir::Results results_;
  type::Type const *type_;
};
}  // namespace ast

#endif  // ICARUS_AST_TERMINAL_H
