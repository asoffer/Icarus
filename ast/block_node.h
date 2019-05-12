#ifndef ICARUS_AST_BLOCK_NODE_H
#define ICARUS_AST_BLOCK_NODE_H

#include <memory>
#include "ast/expression.h"
#include "ast/statements.h"
#include "misc/context.h"
#include "core/scope.h"

namespace ast {
struct BlockNode : public Expression {
  BlockNode() = default;
  ~BlockNode() override {}
  BlockNode(BlockNode &&) noexcept = default;
  BlockNode &operator=(BlockNode &&) noexcept = default;

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::unique_ptr<Expression> name_;
  Statements stmts_;
  std::unique_ptr<core::ExecScope> block_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_NODE_H
