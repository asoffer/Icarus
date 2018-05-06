#include "ast/scope_node.h"

#include <sstream>
#include "ast/block_literal.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/scope.h"
#include "type/type.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
using base::check::Is;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  for (const auto &block : blocks_) {
    ss << block->to_string(n) << " {";
    auto &stmts = block_map_.at(block.get()).stmts_;
    if (stmts.content_.size() > 1) { ss << "\n"; }
    ss << stmts.to_string(n + 1) << "} ";
  }
  return ss.str();
}

void ScopeNode::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto & [ block_expr, block_node ] : block_map_) {
    block_expr->assign_scope(scope);
    block_node.block_scope_ = scope_->add_child<ExecScope>();
    block_node.stmts_.assign_scope(block_node.block_scope_.get());
  }
}

void ScopeNode::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto & [ block_expr, block_node ] : block_map_) {
    block_expr->ClearIdDecls();
    block_node.stmts_.ClearIdDecls();
  }
}

void ScopeNode::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::RVal;

  for (auto & [ block_expr, block_node ] : block_map_) {
    block_node.stmts_.VerifyType(ctx);
    limit_to(&block_node.stmts_);
  }

  blocks_[0]->VerifyType(ctx);
  limit_to(blocks_[0]);
  if (!blocks_[0]->type->is<type::Scope>()) { NOT_YET("not a scope"); }

  type = type::Void;  // TODO can this evaluate to anything?
}

void ScopeNode::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  // TODO
}

void ScopeNode::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto & [ block_expr, block_node ] : block_map_) {
    block_expr->SaveReferences(scope, args);
    block_node.stmts_.SaveReferences(scope, args);
  }
}

void ScopeNode::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  const auto &corr = correspondant->as<ScopeNode>();
  for (size_t i = 0; i < blocks_.size(); ++i) {
    blocks_[i]->contextualize(corr.blocks_[i].get(), replacements);
    block_map_[blocks_[i].get()].stmts_.contextualize(
        &corr.block_map_.at(blocks_[i].get()).stmts_, replacements);
  }
}

void ScopeNode::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (const auto & [ block_expr, block_node ] : block_map_) {
    block_expr->ExtractReturns(rets);
    block_node.stmts_.ExtractReturns(rets);
  }
}

ScopeNode *ScopeNode::Clone() const {
  auto *result = new ScopeNode;
  result->span = span;

  result->blocks_.reserve(blocks_.size());
  for (const auto &block_expr : blocks_) {
    result->blocks_.emplace_back(block_expr->Clone());
    const auto &block_node = block_map_.at(block_expr.get());
    result->block_map_[result->blocks_.back().get()] =
        BlockNode{Statements{block_node.stmts_}, nullptr};
  }
  return result;
}

IR::Val AST::ScopeNode::EmitIR(Context *ctx) {
  IR::Val scope_expr_val = Evaluate(blocks_[0].get(), ctx)[0];
  ASSERT(scope_expr_val.type, Is<type::Scope>());
  auto *scope_lit = std::get<ScopeLiteral *>(scope_expr_val.value);

  auto land_block = IR::Func::Current->AddBlock();

  struct BlockData {
    IR::BlockIndex before, body, after;
  };
  std::unordered_map<AST::BlockLiteral *, BlockData> lit_to_data;
  std::unordered_map<std::string, BlockData *> name_to_data;
  for (const auto &decl : scope_lit->decls_) {
    if (decl.type != type::Block) { NOT_YET("local variables"); }
    auto block_val  = Evaluate(decl.init_val.get(), ctx)[0];
    auto block_lit  = std::get<AST::BlockLiteral *>(block_val.value);
    auto[iter, success] =
        lit_to_data.emplace(block_lit, BlockData{
                                           IR::Func::Current->AddBlock(),
                                           IR::Func::Current->AddBlock(),
                                           IR::Func::Current->AddBlock(),
                                       });
    ASSERT(success);
    auto* block_data = &iter->second;

    if (decl.identifier->token == "self") {
      // TODO check constness as part of type-checking
      IR::UncondJump(block_data->before);
      ASSERT(blocks_[0], Is<Identifier>());
      name_to_data.emplace(blocks_[0]->as<Identifier>().token, block_data);
    } else {
      name_to_data.emplace(decl.identifier->token, block_data);
    }
  }

  std::unordered_map<BlockData *, Statements *> data_to_stmts;
  for (const auto &block : blocks_) {
    ASSERT(block, Is<Identifier>());
    auto *block_data = name_to_data.at(block->as<Identifier>().token);
    data_to_stmts.emplace(block_data, &block_map_.at(block.get()).stmts_);
  }
  name_to_data.clear();


  for (auto & [ block_lit, block_data ] : lit_to_data) {
    IR::BasicBlock::Current = block_data.before;
    // TODO the context for the before_ and after_ functions is wrong. Bound
    // constants should not be for those in this scope but in the block literal
    // scope.
    auto call_enter_result = IR::Call(block_lit->before_->EmitIR(ctx), {}, {});
    for (auto & [ jump_block_lit, jump_block_data ] : lit_to_data) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.body,
          IR::Eq(call_enter_result, IR::Val::Block(jump_block_lit)));
    }
    // TODO we're not checking that this is an exit block but we probably
    // should.
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = block_data.body;
    data_to_stmts.at(&block_data)->EmitIR(ctx);
    IR::UncondJump(block_data.after);

    IR::BasicBlock::Current = block_data.after;
    auto call_exit_result = IR::Call(block_lit->after_->EmitIR(ctx), {}, {});
    for (auto & [ jump_block_lit, jump_block_data ] : lit_to_data) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.before,
          IR::Eq(call_exit_result, IR::Val::Block(jump_block_lit)));
    }
    // TODO we're not checking that this is an exit block but we probably
    // should.
    IR::UncondJump(land_block);
  }

  IR::BasicBlock::Current = land_block;

  return IR::Val::None();
}

IR::Val ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
