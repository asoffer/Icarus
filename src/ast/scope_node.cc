#include "ast/scope_node.h"

#include <sstream>
#include "ast/block_literal.h"
#include "ast/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "ir/block_sequence.h"
#include "ir/func.h"
#include "scope.h"
#include "type/scope.h"
#include "type/type.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

std::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace AST {
using base::check::Is;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  for (const auto &block : blocks_) {
    ss << block->to_string(n);
    const auto &block_node = block_map_.at(block.get());
    if (block_node.arg_ != nullptr) {
      ss << " (" << block_node.arg_->to_string(n) << ")";
    }
    ss << " {";
    if (block_node.stmts_.content_.size() > 1) { ss << "\n"; }
    ss << block_node.stmts_.to_string(n + 1) << "} ";
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
    if (block_node.arg_) {
      block_node.arg_->assign_scope(block_node.block_scope_.get());
    }
  }
}

void ScopeNode::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto & [ block_expr, block_node ] : block_map_) {
    block_expr->ClearIdDecls();
    block_node.stmts_.ClearIdDecls();
    if (block_node.arg_) { block_node.arg_->ClearIdDecls(); }
  }
}

void ScopeNode::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::RVal;

  for (auto & [ block_expr, block_node ] : block_map_) {
    block_node.stmts_.VerifyType(ctx);
    limit_to(&block_node.stmts_);
    if (block_node.arg_) {
      block_node.arg_->VerifyType(ctx);
      limit_to(block_node.arg_);
    }
  }

  VERIFY_AND_RETURN_ON_ERROR(blocks_[0]);
  if (!blocks_[0]->type->is<type::Scope>()) {
    NOT_YET("not a scope", blocks_[0]->type);
  }

  type = type::Void();  // TODO can this evaluate to anything?
}

void ScopeNode::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  // TODO
}

void ScopeNode::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto & [ block_expr, block_node ] : block_map_) {
    block_expr->SaveReferences(scope, args);
    block_node.stmts_.SaveReferences(scope, args);
    if (block_node.arg_) { block_node.arg_->SaveReferences(scope, args); }
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
    if (block_map_[blocks_[i].get()].arg_) {
      block_map_[blocks_[i].get()].arg_->contextualize(
          corr.block_map_.at(blocks_[i].get()).arg_.get(), replacements);
    }
  }
}

void ScopeNode::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (const auto & [ block_expr, block_node ] : block_map_) {
    block_expr->ExtractReturns(rets);
     block_node.stmts_.ExtractReturns(rets);
     if (block_node.arg_) { block_node.arg_->ExtractReturns(rets); }
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
        BlockNode{Statements{block_node.stmts_},
                  block_node.arg_ == nullptr
                      ? nullptr
                      : base::wrap_unique(block_node.arg_->Clone()),
                  nullptr};
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
  for (const auto & [ expr, block_node ] : block_map_) {
    // TODO this probably doesn't always have to be an identifier.
    const auto& block_node_name = expr->as<Identifier>().token;
    // TODO better search

    for (const auto &decl : scope_lit->decls_) {
      if (decl.identifier->token != block_node_name &&
          !(decl.identifier->token == "self" && expr == blocks_[0].get())) {
        continue;
      }
      auto block_val = Evaluate(decl.init_val.get(), ctx)[0];
      auto block_seq = std::get<IR::BlockSequence>(block_val.value);
      ASSERT(block_seq.seq_.size() == 1u);
      auto *block_lit = block_seq.seq_[0];
      auto[iter, success] =
          lit_to_data.emplace(block_lit, BlockData{
                                             IR::Func::Current->AddBlock(),
                                             IR::Func::Current->AddBlock(),
                                             IR::Func::Current->AddBlock(),
                                         });
      ASSERT(success);
      auto *block_data = &iter->second;

      if (decl.identifier->token == "self") {
        // TODO check constness as part of type-checking
        IR::UncondJump(block_data->before);
        ASSERT(blocks_[0], Is<Identifier>());
        name_to_data.emplace(blocks_[0]->as<Identifier>().token, block_data);
      } else {
        name_to_data.emplace(decl.identifier->token, block_data);
      }
      break;
    }
  }

  std::unordered_map<BlockData *, BlockNode *> data_to_node;
  for (const auto &block : blocks_) {
    ASSERT(block, Is<Identifier>());
    auto *block_data = name_to_data.at(block->as<Identifier>().token);
    data_to_node.emplace(block_data, &block_map_.at(block.get()));
  }
  name_to_data.clear();

  for (auto & [ block_lit, block_data ] : lit_to_data) {
    IR::BasicBlock::Current = block_data.before;
    // TODO the context for the before_ and after_ functions is wrong. Bound
    // constants should not be for those in this scope but in the block literal
    // scope.
    auto iter = data_to_node.find(&block_data);
    if (iter == data_to_node.end()) { continue; }
    auto* arg_expr = iter->second->arg_.get();

    auto call_enter_result = [&] {
      FnArgs<std::pair<Expression *, IR::Val>> args;
      FnArgs<Expression *> expr_args;
      if (arg_expr != nullptr) {
        ForEachExpr(arg_expr,
                    [ctx, &args, &expr_args](size_t, Expression *expr) {
                      args.pos_.emplace_back(expr, expr->EmitIR(ctx));
                      expr_args.pos_.push_back(expr);
                    });
      }

      auto[dispatch_table, result_type] =
          DispatchTable::Make(expr_args, block_lit->before_.get(), ctx);

      return EmitCallDispatch(args, dispatch_table, result_type, ctx)[0];
    }();

    for (auto & [ jump_block_lit, jump_block_data ] : lit_to_data) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.body,
          IR::BlockSeqContains(call_enter_result, jump_block_lit));
    }
    // TODO we're not checking that this is an exit block but we probably
    // should.
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = block_data.body;
    data_to_node.at(&block_data)->stmts_.EmitIR(ctx);
    IR::UncondJump(block_data.after);

    IR::BasicBlock::Current = block_data.after;

    auto call_exit_result = [&] {
      auto[dispatch_table, result_type] = DispatchTable::Make(
          FnArgs<Expression *>{}, block_lit->after_.get(), ctx);

      return EmitCallDispatch(FnArgs<std::pair<Expression *, IR::Val>>{},
                              dispatch_table, result_type, ctx)[0];
    }();
    for (auto & [ jump_block_lit, jump_block_data ] : lit_to_data) {
      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          jump_block_data.before,
          IR::BlockSeqContains(call_exit_result, jump_block_lit));
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
