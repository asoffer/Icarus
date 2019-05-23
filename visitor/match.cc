#include "visitor/match.h"

#include "ast/ast.h"
#include "match/binding_node.h"

namespace visitor {

void Match::MatchExpr(ast::Node const *node, ast::Expression const *pattern,
                      bool allow_submatch) {
  if (pattern->is<match::BindingNode>()) { base::Log() << node->to_string(0); }
}

void Match::MatchExpr(ast::Access const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::ArrayLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::ArrayType const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Binop const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::BlockLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::BlockNode const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::BuiltinFn const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Call const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::Cast const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::ChainOp const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::CommaList const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Declaration const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::EnumLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::FunctionLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Identifier const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Import const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::Index const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::Interface const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::RepeatedUnop const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::ScopeLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::ScopeNode const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Statements const *node,
                      ast::Expression const *pattern, bool allow_submatch) {
  if (allow_submatch) {
    for (auto const &stmt : node->content_) {
      stmt->match_expr(this, pattern, allow_submatch);
    }
  }
  // TODO determine if this is right: Should a single statement that happens
  // to be an expression match an expression? For now, no, but I'm really not
  // sure.
}

void Match::MatchExpr(ast::StructLiteral const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::StructType const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}

void Match::MatchExpr(ast::Switch const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::SwitchWhen const *node,
                      ast::Expression const *pattern, bool allow_submatch) {}
void Match::MatchExpr(ast::Terminal const *node, ast::Expression const *pattern,
                      bool allow_submatch) {}

void Match::MatchExpr(ast::Unop const *node, ast::Expression const *pattern,
                      bool allow_submatch) {
  auto *unop_pattern = pattern->if_as<ast::Unop>();
  if (allow_submatch) {
    if (unop_pattern) {
      if (unop_pattern->op == node->op) {
        node->operand->match_expr(this, unop_pattern->operand.get(), false);
      }
      node->operand->match_expr(this, pattern, false);
    } else {
      node->operand->match_expr(this, pattern, false);
    }
  } else {
    if (unop_pattern) {
      if (unop_pattern->op == node->op) {
        node->operand->match_expr(this, unop_pattern->operand.get(), false);
      }
    } else if (auto *bind = pattern->if_as<match::BindingNode>()) {
      base::Log() << node->to_string(0);
    }
  }
}

}  // namespace visitor
