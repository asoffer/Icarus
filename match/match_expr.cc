#include "match/match_expr.h"

#include "ast/ast.h"
#include "match/binding_node.h"

namespace match {

void Match::MatchAll(ast::Node const *node, ast::Expression const *pattern) {
  states_.emplace(node, pattern);
  while (not states_.empty()) {
    auto *state = &states_.front();
    Visit(state->current_node_, state);
    states_.pop();
  }
}

void Match::Visit(ast::Node const *node, MatchState *state) {
  if (state->current_pattern_->is<match::BindingNode>()) {
    DEBUG_LOG()(node->DebugString());
  }
}

void Match::Visit(ast::Access const *node, MatchState *state) {
  if (auto *ac = state->current_pattern_->if_as<ast::Access>()) {
    MatchState new_state       = *state;
    new_state.current_node_    = node->operand();
    new_state.current_pattern_ = ac->operand();
    states_.push(new_state);
  } else if (auto *binding =
                 state->current_pattern_->if_as<match::BindingNode>()) {
    DEBUG_LOG()(node->DebugString());
  }

  if (not state->root_) {
    MatchState new_state    = *state;
    new_state.current_node_ = node->operand();
    states_.push(new_state);
  }
}

void Match::Visit(ast::ArrayLiteral const *node, MatchState *state) {}

void Match::Visit(ast::ArgumentType const *node, MatchState *state) {}

void Match::Visit(ast::ArrayType const *node, MatchState *state) {}

void Match::Visit(ast::Binop const *node, MatchState *state) {}

void Match::Visit(ast::BlockLiteral const *node, MatchState *state) {}

void Match::Visit(ast::BlockNode const *node, MatchState *state) {}

void Match::Visit(ast::BuiltinFn const *node, MatchState *state) {}

void Match::Visit(ast::Call const *node, MatchState *state) {}

void Match::Visit(ast::Cast const *node, MatchState *state) {}

void Match::Visit(ast::ChainOp const *node, MatchState *state) {}

void Match::Visit(ast::CommaList const *node, MatchState *state) {}

void Match::Visit(ast::Declaration const *node, MatchState *state) {}

void Match::Visit(ast::DesignatedInitializer const *node, MatchState *state) {}

void Match::Visit(ast::EnumLiteral const *node, MatchState *state) {}

void Match::Visit(ast::FunctionLiteral const *node, MatchState *state) {}

void Match::Visit(ast::ShortFunctionLiteral const *node, MatchState *state) {}

void Match::Visit(ast::Identifier const *node, MatchState *state) {}

void Match::Visit(ast::Import const *node, MatchState *state) {}

void Match::Visit(ast::Index const *node, MatchState *state) {}

void Match::Visit(ast::Goto const *node, MatchState *state) {}

void Match::Visit(ast::Label const *node, MatchState *state) {}

void Match::Visit(ast::Jump const *node, MatchState *state) {}

void Match::Visit(ast::YieldStmt const *node, MatchState *state) {}

void Match::Visit(ast::ReturnStmt const *node, MatchState *state) {}

void Match::Visit(ast::ScopeLiteral const *node, MatchState *state) {}

void Match::Visit(ast::ScopeNode const *node, MatchState *state) {}

void Match::Visit(ast::StructLiteral const *node, MatchState *state) {}

void Match::Visit(ast::ParameterizedStructLiteral const *node,
                  MatchState *state) {}

void Match::Visit(ast::StructType const *node, MatchState *state) {}

void Match::Visit(ast::Switch const *node, MatchState *state) {}

void Match::Visit(ast::Terminal const *node, MatchState *state) {}

void Match::Visit(ast::Unop const *node, MatchState *state) {}

}  // namespace match
