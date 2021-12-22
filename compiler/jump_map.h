#ifndef ICARUS_COMPILER_JUMP_MAP_H
#define ICARUS_COMPILER_JUMP_MAP_H

#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/ptr_union.h"

namespace compiler {

// Represents a mapping from an AST node `n` to the nodes which may yield
// control flow to `n`.
//
// * If `n` is a function literal, the mapped nodes will be return statements.
// * If `n` is a jump literal, the mapped nodes will be goto statements.
// * If `n` is a block or scope node, the mapped nodes will be yield statements.
struct JumpMap {
  void TrackJumps(ast::Node const *p);

  std::vector<ast::ReturnStmt const *> const *operator[](
      base::PtrUnion<ast::FunctionLiteral const,
                     ast::ShortFunctionLiteral const>
          p) const {
    auto iter = returns_.find(p);
    return iter != returns_.end() ? &iter->second : nullptr;
  }

  std::vector<ast::YieldStmt const *> const *operator[](
      base::PtrUnion<ast::BlockNode const, ast::ScopeNode const,
                     ast::IfStmt const, ast::WhileStmt const>
          p) const {
    auto iter = yields_.find(p);
    return iter != yields_.end() ? &iter->second : nullptr;
  }

 private:
  struct NodeExtractor;

  void Insert(base::PtrUnion<ast::FunctionLiteral const,
                             ast::ShortFunctionLiteral const>
                  node,
              ast::ReturnStmt const *r) {
    returns_[node].push_back(r);
  }

  void Insert(base::PtrUnion<ast::BlockNode const, ast::ScopeNode const,
                             ast::IfStmt const, ast::WhileStmt const>
                  node,
              ast::YieldStmt const *y) {
    yields_[node].push_back(y);
  }

  absl::flat_hash_map<base::PtrUnion<ast::FunctionLiteral const,
                                     ast::ShortFunctionLiteral const>,
                      std::vector<ast::ReturnStmt const *>>
      returns_;

  absl::flat_hash_map<base::PtrUnion<ast::BlockNode const, ast::ScopeNode const,
                                     ast::IfStmt const, ast::WhileStmt const>,
                      std::vector<ast::YieldStmt const *>>
      yields_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_JUMP_MAP_H
