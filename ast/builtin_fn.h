#ifndef ICARUS_AST_BUILTIN_FN_H
#define ICARUS_AST_BUILTIN_FN_H

#include "ast/expression.h"
#include "core/fn_args.h"
#include "ir/builtin.h"

namespace ast {

struct BuiltinFn : public Expression {
  BuiltinFn() = default;
  BuiltinFn(const TextSpan &span, ir::Builtin b) : Expression(span), b_(b) {}
  ~BuiltinFn() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t) const override { return stringify(b_); }

  ir::Builtin b_;
};

}  // namespace ast

#endif  // ICARUS_AST_BUILTIN_FN_H
