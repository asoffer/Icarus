#ifndef ICARUS_AST_BUILTIN_FN_H
#define ICARUS_AST_BUILTIN_FN_H

#include "ast/expression.h"
#include "ir/builtin.h"

namespace ast {

struct BuiltinFn : public Expression {
  BuiltinFn() = default;
  BuiltinFn(const TextSpan &span, ir::Builtin b) : Expression(span), b_(b) {}
  ~BuiltinFn() override {}

#include "visitor/visitors.xmacro.h"

  ir::Builtin b_;
};

}  // namespace ast

#endif  // ICARUS_AST_BUILTIN_FN_H
