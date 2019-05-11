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

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t) const override { return stringify(b_); }

  ast_visitor::VerifyResult VerifyCall(
      core::FnArgs<std::unique_ptr<Expression>> const &args,
      core::FnArgs<std::pair<Expression const *,
                             ast_visitor::VerifyResult>> const &arg_results,
      Context *ctx) const;

  // TODO distinguish between guaranteed failures and failures to continue
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return type::Type_ && ir::BuiltinType(b_) == t;
  }

  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override {}

  ir::Results EmitIr(Context *ctx) override { return ir::Results{b_}; };

  ir::Builtin b_;
};

}  // namespace ast

#endif  // ICARUS_AST_BUILTIN_FN_H
