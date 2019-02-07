#ifndef ICARUS_AST_SCOPE_NODE_H
#define ICARUS_AST_SCOPE_NODE_H

#include <memory>
#include "ast/block_node.h"
#include "ast/expression.h"
#include "ast/fn_args.h"
#include "ast/statements.h"
#include "base/container/unordered_map.h"
#include "misc/scope.h"

struct Context;

namespace ast {
struct ScopeNode : public Expression {
  ~ScopeNode() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  
  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::unique_ptr<Expression> name_;
  FnArgs<std::unique_ptr<Expression>> args_;
  base::vector<BlockNode> blocks_;
  ScopeNode *sugared_ = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_SCOPE_NODE_H
