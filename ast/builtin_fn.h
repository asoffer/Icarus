#ifndef ICARUS_AST_BUILTIN_FN_H
#define ICARUS_AST_BUILTIN_FN_H

#include "core/fn_args.h"
#include "ast/literal.h"
#include "ir/builtin.h"
#include "misc/module.h"
#include "misc/context.h"

struct Context;

namespace ast {

struct BuiltinFn : public Literal {
  BuiltinFn() = default;
  BuiltinFn(const TextSpan &span, ir::Builtin b) : Literal(span), b_(b) {}
  ~BuiltinFn() override {}

  void assign_scope(core::Scope *scope) override { scope_ = scope; }
  std::string to_string(size_t) const override { return stringify(b_); }

  VerifyResult VerifyType(Context *ctx) override {
    return ctx->set_result(this, VerifyResult::Constant(ir::BuiltinType(b_)));
  }

  VerifyResult VerifyCall(core::FnArgs<std::unique_ptr<Expression>> const &args,
                          core::FnArgs<VerifyResult> const &arg_results,
                          Context *ctx) const;

  // TODO distinguish between guaranteed failures and failures to continue
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return type::Type_ && ir::BuiltinType(b_) == t;
  }

  void ExtractJumps(JumpExprs *) const override {}
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override {}

  ir::Results EmitIr(Context *ctx) override { return ir::Results{b_}; };

  ir::Builtin b_;
};

}  // namespace ast

#endif  // ICARUS_AST_BUILTIN_FN_H
