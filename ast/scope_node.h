#ifndef ICARUS_AST_SCOPE_NODE_H
#define ICARUS_AST_SCOPE_NODE_H

#include <memory>
#include "ast/block_node.h"
#include "ast/expression.h"
#include "core/fn_args.h"
#include "ast/statements.h"
#include "core/scope.h"

struct Context;

namespace ast {
struct ScopeNode : public Expression {
  ~ScopeNode() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << name_->to_string(n) << " ";
    if (!args_.empty()) { ss << "(" << args_.to_string() << ") "; }
    for (auto const &block : blocks_) { ss << block.to_string(n); }
    return ss.str();
  }

  std::unique_ptr<Expression> name_;
  core::FnArgs<std::unique_ptr<Expression>> args_;
  std::vector<BlockNode> blocks_;
  ScopeNode *sugared_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_SCOPE_NODE_H
