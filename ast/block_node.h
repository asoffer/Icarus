#ifndef ICARUS_AST_BLOCK_NODE_H
#define ICARUS_AST_BLOCK_NODE_H

#include <memory>
#include "ast/expression.h"
#include "ast/statements.h"
#include "core/scope.h"

namespace ast {
struct BlockNode : public Expression {
  BlockNode() = default;
  ~BlockNode() override {}
  BlockNode(BlockNode &&) noexcept = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << name_->to_string(n) << " {\n"
       << stmts_.to_string(n + 1) << std::string(2 * n, ' ') << "} ";
    return ss.str();
  }

  std::unique_ptr<Expression> name_;
  Statements stmts_;
  std::unique_ptr<core::ExecScope> block_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_NODE_H
