#include "ast/scope_node.h"

#include <sstream>
#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "ast/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/scope.h"
#include "type/type.h"

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace AST {
using base::check::Is;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " ";
  if (!args_.empty()) { ss << "(" << args_.to_string(n) << ") "; }
  for (auto const &block : blocks_) { ss << block->to_string(n); }
  return ss.str();
}

void ScopeNode::assign_scope(Scope *scope) {
  scope_ = scope;
  name_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
  for (auto &block : blocks_) { block->assign_scope(scope); }
}

type::Type const *ScopeNode::VerifyType(Context *ctx) {
  for (auto &block : blocks_) {
    limit_to(&block->stmts_);
    if (block->arg_) {
      block->arg_->VerifyType(ctx);
      limit_to(block->arg_);
    }
  }

  // TODO check that all the blocks make sense and emit errors

  ctx->set_type(this, type::Void());
  return type::Void();  // TODO can this evaluate to anything?
}

void ScopeNode::Validate(Context *ctx) {
  for (auto &block_node : blocks_) {
    block_node->stmts_.VerifyType(ctx);
    block_node->stmts_.Validate(ctx);
  }
  // TODO
}

void ScopeNode::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  NOT_YET();
}

void ScopeNode::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  const auto &corr = correspondant->as<ScopeNode>();
  NOT_YET();
}

void ScopeNode::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &block : blocks_) { block->ExtractReturns(rets); }
}

ScopeNode *ScopeNode::Clone() const {
  auto *result = new ScopeNode;
  result->span = span;

  NOT_YET();
  return result;
}

// TODO make static again
/* static */ std::pair<const Module *, std::string> GetQualifiedIdentifier(
    Expression *expr, Context *ctx) {
  if (expr->is<Identifier>()) {
    const auto &token = expr->as<Identifier>().token;
    return std::pair{ctx->mod_, token};
  } else if (expr->is<Access>()) {
    auto *access = &expr->as<Access>();
    auto *mod = backend::EvaluateAs<const Module *>(access->operand.get(), ctx);
    return std::pair{mod, access->member_name};
  }
  UNREACHABLE(expr->to_string(0));
}

base::vector<IR::Val> AST::ScopeNode::EmitIR(Context *ctx) {
  // TODO
  // auto *scope_lit = backend::EvaluateAs<ScopeLiteral *>(blocks_[0].get(), ctx);
  // auto land_block = IR::Func::Current->AddBlock();

  return {};
  /*
  struct BlockData {
    IR::BlockIndex before, body, after;
  };
  base::unordered_map<AST::BlockLiteral *, BlockData> lit_to_data;
  base::unordered_map<std::string, BlockData *> name_to_data;
  std::string top_block_node_name;
  for (auto const & [ expr, block_node ] : block_map_) {
    auto[mod, block_node_name] = GetQualifiedIdentifier(expr, ctx);

    // TODO better search

    for (auto const &decl : scope_lit->decls_) {
      if (decl.id_ != block_node_name &&
          !(decl.id_ == "self" && expr == blocks_[0].get())) {
        continue;
      }
      auto block_seq =
          backend::EvaluateAs<IR::BlockSequence>(decl.init_val.get(), ctx);
      ASSERT(block_seq.seq_->size() == 1u);
      auto *block_lit = block_seq.seq_->front();
      auto[iter, success] =
          lit_to_data.emplace(block_lit, BlockData{
                                             IR::Func::Current->AddBlock(),
                                             IR::Func::Current->AddBlock(),
                                             IR::Func::Current->AddBlock(),
                                         });
      ASSERT(success);
      auto *block_data = &iter->second;

      if (decl.id_ == "self") {
        // TODO check constness as part of type-checking
        IR::UncondJump(block_data->before);
        top_block_node_name = block_node_name;
        name_to_data.emplace(block_node_name, block_data);
      } else {
        name_to_data.emplace(decl.id_, block_data);
      }
      break;
    }
  }

  // TODO can we just store "self" in name_to_data to avoid this nonsense?
  base::unordered_map<BlockData *, BlockNode *> data_to_node;
  for (auto const &block : blocks_) {
    auto *block_data = block.is<Identifier>()
                           ? name_to_data.at(block.as<Identifier>().token)
                           : name_to_data.at(top_block_node_name);
    data_to_node.emplace(block_data, &block_map_.at(&block));
  }
  name_to_data.clear();

  for (auto & [ block_lit, block_data ] : lit_to_data) {
    IR::BasicBlock::Current = block_data.before;
    // TODO the context for the before_ and after_ functions is wrong. Bound
    // constants should not be for those in this scope but in the block literal
    // scope.
    auto iter = data_to_node.find(&block_data);
    if (iter == data_to_node.end()) { continue; }
    auto *arg_expr = iter->second->arg_.get();

    std::unordered_set<type::Type const *> state_types;
    OverloadSet os_before;
    for (auto &b : block_lit->before_) {
      auto *t = ctx->type_of(b.get());
      os_before.emplace_back(b.get(), t);
      state_types.insert(t->as<type::Function>().input[0]);
    }

    // TODO do this at type-checking
    ASSERT(state_types.size() == 1u);
    auto *state_ptr_type = *state_types.begin();
    ASSERT(state_ptr_type, Is<type::Pointer>());

    IR::Register alloc =
        IR::Alloca(state_ptr_type->as<type::Pointer>().pointee);
    // We need to keep this around for the life of module. It's possible we
    // could kill it earlier, but it's probably not a huge deal either way.
    auto *state_id = new Identifier(TextSpan{}, "<scope-state>");
    ctx->set_type(state_id, state_ptr_type);

    auto call_enter_result = [&] {
      FnArgs<std::pair<Expression *, IR::Val>> args;
      args.pos_.emplace_back(state_id, IR::Val::Reg(alloc, state_ptr_type));
      FnArgs<Expression *> expr_args;
      expr_args.pos_.push_back(state_id);
      if (arg_expr != nullptr) {
        ForEachExpr(arg_expr, [&](size_t, Expression *expr) {
          args.pos_.emplace_back(expr, expr->EmitIR(ctx)[0]);
          expr_args.pos_.push_back(expr);
        });
      }

      auto[dispatch_table, result_type] =
          DispatchTable::Make(expr_args, os_before, ctx);

      return EmitCallDispatch(args, dispatch_table, result_type, ctx)[0]
          .reg_or<IR::BlockSequence>();
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
    data_to_node.at(&block_data)->stmts_.EmitIR(ctx)[0];
    IR::UncondJump(block_data.after);

    IR::BasicBlock::Current = block_data.after;

    auto call_exit_result = [&] {
      FnArgs<std::pair<Expression *, IR::Val>> args;
      args.pos_.emplace_back(state_id, IR::Val::Reg(alloc, state_ptr_type));
      FnArgs<Expression *> expr_args;
      expr_args.pos_.push_back(state_id);

      OverloadSet os_after;
      for (auto &a : block_lit->after_) {
        os_after.emplace_back(a.get(), ctx->type_of(a.get()));
      }
      auto[dispatch_table, result_type] =
          DispatchTable::Make(expr_args, os_after, ctx);
      return EmitCallDispatch(args, dispatch_table, result_type, ctx)[0]
          .reg_or<IR::BlockSequence>();
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

  return {};
  */
}

base::vector<IR::Register> ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
