#ifndef ICARUS_COMPILER_JUMP_MAP_H
#define ICARUS_COMPILER_JUMP_MAP_H

#include <variant>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "ast/ast.h"

namespace compiler {

// Represents a mapping from an AST node `n` to the nodes which may yield
// control flow to `n`.
//
// * If `n` is a function literal, the mapped nodes will be return statements.
// * If `n` is a jump literal, the mapped nodes will be goto statements.
// * If `n` is a block or scope node, the mapped nodes will be yield statements.
struct JumpMap {
  void TrackJumps(ast::Node const *p);

  // Returns a pointer to a vector containing all return stma
  std::vector<ast::ReturnStmt const *> const *operator[](
      ast::FunctionLiteral const *p) const {
    auto iter = returns_.find(p);
    return iter != returns_.end() ? &iter->second : nullptr;
  }

  std::vector<ast::ReturnStmt const *> const *operator[](
      ast::ShortFunctionLiteral const *p) const {
    auto iter = returns_.find(p);
    return iter != returns_.end() ? &iter->second : nullptr;
  }

  std::vector<ast::YieldStmt const *> const *operator[](
      ast::BlockNode const *p) const {
    auto iter = yields_.find(p);
    return iter != yields_.end() ? &iter->second : nullptr;
  }

  std::vector<ast::YieldStmt const *> const *operator[](
      ast::ScopeNode const *p) const {
    auto iter = yields_.find(p);
    return iter != yields_.end() ? &iter->second : nullptr;
  }

  std::vector<std::variant<ast::UnconditionalGoto const *,
                           ast::ConditionalGoto const *>> const *
  operator[](ast::Jump const *p) const {
    auto iter = gotos_.find(p);
    return iter != gotos_.end() ? &iter->second : nullptr;
  }

 private:
  struct NodeExtractor;

  void Insert(ast::FunctionLiteral const *node, ast::ReturnStmt const *r) {
    returns_[node].push_back(r);
  }

  void Insert(ast::ShortFunctionLiteral const *node, ast::ReturnStmt const *r) {
    returns_[node].push_back(r);
  }

  void Insert(ast::BlockNode const *node, ast::YieldStmt const *y) {
    yields_[node].push_back(y);
  }

  void Insert(ast::ScopeNode const *node, ast::YieldStmt const *y) {
    yields_[node].push_back(y);
  }

  void Insert(
      ast::Jump const *node,
      std::variant<ast::UnconditionalGoto const *, ast::ConditionalGoto const *>
          g) {
    gotos_[node].push_back(g);
  }

  absl::flat_hash_map<ast::ParameterizedExpression const *,
                      std::vector<ast::ReturnStmt const *>>
      returns_;

  absl::flat_hash_map<ast::Expression const *,
                      std::vector<ast::YieldStmt const *>>
      yields_;

  // TODO: Add a base::PtrUnion to condense these into a single pointer-sized
  // value, using the spare bits to distinguish.
  absl::flat_hash_map<ast::Jump const *,
                      std::vector<std::variant<ast::UnconditionalGoto const *,
                                               ast::ConditionalGoto const *>>>
      gotos_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_JUMP_MAP_H
