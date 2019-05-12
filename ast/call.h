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

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::unique_ptr<Expression> fn_;  // Rename to `callable_` or something
  core::FnArgs<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
