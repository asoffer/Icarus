#include "match/match_expr.h"

#include "ast/ast.h"
#include "ast/methods/dump.h"
#include "match/binding_node.h"

namespace match {

void Match::MatchAll(ast::Node const *node, ast::Expression const *pattern) {
  states_.emplace(node, pattern);
  while (not states_.empty()) {
    auto *state = &states_.front();
    state->current_node_->match_expr(this, state);
    states_.pop();
  }
}

void Match::MatchExpr(ast::Node const *node, Match::State *state) {
  if (state->current_pattern_->is<match::BindingNode>()) {
    DEBUG_LOG()(ast::Dump::ToString(node));
  }
}

void Match::MatchExpr(ast::Access const *node, Match::State *state) {
  if (auto *ac = state->current_pattern_->if_as<ast::Access>()) {
    Match::State new_state     = *state;
    new_state.current_node_    = node->operand();
    new_state.current_pattern_ = ac->operand();
    states_.push(new_state);
  } else if (auto *binding =
                 state->current_pattern_->if_as<match::BindingNode>()) {
    DEBUG_LOG()(ast::Dump::ToString(node));
  }

  if (not state->root_) {
    Match::State new_state  = *state;
    new_state.current_node_ = node->operand();
    states_.push(new_state);
  }
}

void Match::MatchExpr(ast::ArrayLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::ArrayType const *node, Match::State *state) {}

void Match::MatchExpr(ast::Binop const *node, Match::State *state) {}

void Match::MatchExpr(ast::BlockLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::BlockNode const *node, Match::State *state) {}

void Match::MatchExpr(ast::BuiltinFn const *node, Match::State *state) {}

void Match::MatchExpr(ast::Call const *node, Match::State *state) {}

void Match::MatchExpr(ast::Cast const *node, Match::State *state) {}

void Match::MatchExpr(ast::ChainOp const *node, Match::State *state) {}

void Match::MatchExpr(ast::CommaList const *node, Match::State *state) {}

void Match::MatchExpr(ast::Declaration const *node, Match::State *state) {}

void Match::MatchExpr(ast::EnumLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::FunctionLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::Identifier const *node, Match::State *state) {}

void Match::MatchExpr(ast::Import const *node, Match::State *state) {}

void Match::MatchExpr(ast::Index const *node, Match::State *state) {}

void Match::MatchExpr(ast::Jump const *node, Match::State *state) {}

void Match::MatchExpr(ast::JumpHandler const *node, Match::State *state) {}

void Match::MatchExpr(ast::PrintStmt const *node, Match::State *state) {}

void Match::MatchExpr(ast::YieldStmt const *node, Match::State *state) {}

void Match::MatchExpr(ast::ReturnStmt const *node, Match::State *state) {}

void Match::MatchExpr(ast::ScopeLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::ScopeNode const *node, Match::State *state) {}

void Match::MatchExpr(ast::StructLiteral const *node, Match::State *state) {}

void Match::MatchExpr(ast::StructType const *node, Match::State *state) {}

void Match::MatchExpr(ast::Switch const *node, Match::State *state) {}

void Match::MatchExpr(ast::Terminal const *node, Match::State *state) {}

void Match::MatchExpr(ast::Unop const *node, Match::State *state) {}

}  // namespace match
