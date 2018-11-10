#ifndef ICARUS_AST_SCOPE_NODE_H
#define ICARUS_AST_SCOPE_NODE_H

#include <memory>
#include "ast/block_node.h"
#include "ast/expression.h"
#include "ast/fn_args.h"
#include "ast/statements.h"
#include "base/container/unordered_map.h"
#include "scope.h"

struct Context;

namespace AST {
struct ScopeNode : public Expression {
  ~ScopeNode() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  
  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  std::unique_ptr<Expression> name_;
  FnArgs<std::unique_ptr<Expression>> args_;
  // TODO store by value.
  base::vector<std::unique_ptr<BlockNode>> blocks_;
  ScopeNode *sugared_ = nullptr;
};
}  // namespace AST

#endif  // ICARUS_AST_SCOPE_NODE_H
