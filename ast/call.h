#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "ast/expression.h"
#include "core/fn_args.h"

namespace ast {
struct Call : public Expression {
  Call() = default;
  explicit Call(std::unique_ptr<Expression> fn,
                core::FnArgs<std::unique_ptr<Expression>> args = {})
      : fn_(std::move(fn)), args_(std::move(args)) {}

  ~Call() override {}

  std::pair<std::unique_ptr<Expression>,
            core::FnArgs<std::unique_ptr<Expression>>>
  extract() && {
    return std::pair<std::unique_ptr<Expression>,
                     core::FnArgs<std::unique_ptr<Expression>>>(
        std::move(fn_), std::move(args_));
  }

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<Expression> fn_;  // Rename to `callable_` or something
  core::FnArgs<std::unique_ptr<Expression>> args_;
  // TODO keep info about the syntax was it f(a) or a'f?
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
