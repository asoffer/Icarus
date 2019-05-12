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

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << fn_->to_string(n) << "(";
    bool seen_one = false;
    args_.ApplyWithIndex([&](auto &&index,
                             std::unique_ptr<Expression> const &expr) {
      ss << (seen_one ? ", " : "");
      if constexpr (!std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
        ss << index << " = ";
      }
      ss << expr->to_string(n);
      seen_one = true;
    });
    ss << ")";
    return ss.str();
  }

  std::unique_ptr<Expression> fn_;  // Rename to `callable_` or something
  core::FnArgs<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_CALL_H
