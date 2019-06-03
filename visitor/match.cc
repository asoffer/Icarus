#include "visitor/match.h"

#include "ast/ast.h"
#include "match/binding_node.h"
#include "visitor/dump_ast.h"
#include "visitor/match_state.h"

namespace visitor {
Match::Match() : states_(std::make_unique<std::queue<MatchState>>()) {}
Match::~Match() = default;

void Match::MatchAll(ast::Node const *node, ast::Expression const *pattern) {
  states_->emplace(node, pattern);
  while (!states_->empty()) {
    auto *state = &states_->front();
    state->current_node_->match_expr(this, state);
    states_->pop();
  }
}

void Match::MatchExpr(ast::Node const *node, MatchState *state) {
  if (state->current_pattern_->is<match::BindingNode>()) {
    base::Log() << DumpAst::ToString(node);
  }
}

void Match::MatchExpr(ast::Access const *node, MatchState *state) {
  if (auto *ac = state->current_pattern_->if_as<ast::Access>()) {
    MatchState new_state       = *state;
    new_state.current_node_    = node->operand();
    new_state.current_pattern_ = ac->operand();
    states_->push(new_state);
  } else if (auto *binding =
                 state->current_pattern_->if_as<match::BindingNode>()) {
    base::Log() << node;
  }

  if (!state->root_) {
    MatchState new_state    = *state;
    new_state.current_node_ = node->operand();
    states_->push(new_state);
  }
}

void Match::MatchExpr(ast::ArrayLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::ArrayType const *node, MatchState *state) {}

void Match::MatchExpr(ast::Binop const *node, MatchState *state) {}

void Match::MatchExpr(ast::BlockLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::BlockNode const *node, MatchState *state) {}

void Match::MatchExpr(ast::BuiltinFn const *node, MatchState *state) {}

void Match::MatchExpr(ast::Call const *node, MatchState *state) {}

void Match::MatchExpr(ast::Cast const *node, MatchState *state) {}

void Match::MatchExpr(ast::ChainOp const *node, MatchState *state) {}

void Match::MatchExpr(ast::CommaList const *node, MatchState *state) {}

void Match::MatchExpr(ast::Declaration const *node, MatchState *state) {}

void Match::MatchExpr(ast::EnumLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::FunctionLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::Identifier const *node, MatchState *state) {}

void Match::MatchExpr(ast::Import const *node, MatchState *state) {}

void Match::MatchExpr(ast::Index const *node, MatchState *state) {}

void Match::MatchExpr(ast::Interface const *node, MatchState *state) {}

void Match::MatchExpr(ast::Jump const *node, MatchState *state) {}

void Match::MatchExpr(ast::RepeatedUnop const *node, MatchState *state) {}

void Match::MatchExpr(ast::ScopeLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::ScopeNode const *node, MatchState *state) {}

void Match::MatchExpr(ast::StructLiteral const *node, MatchState *state) {}

void Match::MatchExpr(ast::StructType const *node, MatchState *state) {}

void Match::MatchExpr(ast::Switch const *node, MatchState *state) {}

void Match::MatchExpr(ast::Terminal const *node, MatchState *state) {}

void Match::MatchExpr(ast::Unop const *node, MatchState *state) {}

}  // namespace visitor
