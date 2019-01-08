#include "ast/scope_node.h"

#include <sstream>
#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "ast/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "backend/eval.h"
#include "base/util.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
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

VerifyResult ScopeNode::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), [[maybe_unused]] auto result,
                   name_->VerifyType(ctx));
  // TODO check the scope type makes sense.

  auto arg_types =
      args_.Transform([ctx, this](auto &arg) { return arg->VerifyType(ctx); });
  // TODO type check

  for (auto &block : blocks_) { block->VerifyType(ctx); }

  // TODO check that all the blocks make sense and emit errors

  // TODO compute what type this should return
  // TODO can this evaluate to anything?
  // TODO constant is wrong.
  return VerifyResult::Constant(ctx->set_type(this, type::Void()));
}

void ScopeNode::Validate(Context *ctx) {
  for (auto &block_node : blocks_) {
    block_node->stmts_.VerifyType(ctx);
    block_node->stmts_.Validate(ctx);
  }
  // TODO
}

void ScopeNode::ExtractJumps(JumpExprs *rets) const {
  for (auto &block : blocks_) { block->ExtractJumps(rets); }
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

base::vector<ir::Val> ast::ScopeNode::EmitIR(Context *ctx) {
  ctx->yields_stack_.emplace_back();
  base::defer d([&]() { ctx->yields_stack_.pop_back(); });

  auto *scope_lit = backend::EvaluateAs<ScopeLiteral *>(name_.get(), ctx);

  auto init_block = ir::Func::Current->AddBlock();
  auto land_block = ir::Func::Current->AddBlock();

  ir::UncondJump(init_block);
  ir::BasicBlock::Current = init_block;

  OverloadSet init_os;
  for (auto &decl : scope_lit->decls_) {
    if (decl.id_ == "init") {
      init_os.emplace_back(&decl, &ctx->type_of(&decl)->as<type::Function>());
    }
  }

  std::unordered_map<std::string, Declaration *> name_lookup;
  for (auto &decl : scope_lit->decls_) { name_lookup.emplace(decl.id_, &decl); }

  struct BlockData {
    OverloadSet before_os_, after_os_;
    ir::BlockIndex index_;
    BlockLiteral *lit_;
  };

  auto *jump_table =
      new std::unordered_map<ast::BlockLiteral const *, ir::BlockIndex>{
          {reinterpret_cast<ast::BlockLiteral const *>(0x1), init_block},
          {nullptr, land_block}};

  std::unordered_map<BlockNode *, BlockData> block_data;
  ir::Register alloc;
  type::Type const *state_ptr_type = nullptr, *state_type = nullptr;
  std::unordered_set<type::Type const *> state_types;
  for (auto &block : blocks_) {
    // TODO for now do lookup assuming it's an identifier.
    ASSERT(block->name_, Is<Identifier>());
    auto *decl = name_lookup.at(block->name_->as<Identifier>().token);
    auto &bseq = *backend::EvaluateAs<ir::BlockSequence>(decl, ctx).seq_;
    ASSERT(bseq.size() == 1u);

    OverloadSet os_before;
    for (auto &b : bseq[0]->before_) {
      auto *t = ctx->type_of(b.get());
      os_before.emplace_back(b.get(), t);
      if (scope_lit->stateful_) {
        state_types.insert(t->as<type::Function>().input[0]);
      }
    }

    OverloadSet os_after;
    for (auto &a : bseq[0]->after_) {
      auto *t = ctx->type_of(a.get());
      os_after.emplace_back(a.get(), t);

      if (scope_lit->stateful_) {
        state_types.insert(t->as<type::Function>().input[0]);
      }
    }
    auto block_index        = ir::Func::Current->AddBlock();
    block_data[block.get()] = {std::move(os_before), std::move(os_after),
                               block_index, bseq[0]};
    jump_table->emplace(bseq[0], block_index);
  }

  if (scope_lit->stateful_) {
    ASSERT(state_types.size() == 1u);
    state_ptr_type = *state_types.begin();
    ASSERT(state_ptr_type, Is<type::Pointer>());
    state_type = state_ptr_type->as<type::Pointer>().pointee;
    alloc      = ir::TmpAlloca(state_type, ctx);
    state_type->EmitInit(alloc, ctx);
  }

  auto[dispatch_table, result_type] = DispatchTable::Make(
      args_.Transform(
          [](std::unique_ptr<Expression> const &expr) { return expr.get(); }),
      init_os, ctx);
  auto block_seq =
      dispatch_table
          .EmitCall(
              args_.Transform([ctx](std::unique_ptr<Expression> const &expr) {
                return std::pair(const_cast<Expression *>(expr.get()),
                                 expr->EmitIR(ctx)[0]);
              }),
              result_type, ctx)[0]
          .reg_or<ir::BlockSequence>();
  ir::BlockSeqJump(block_seq, jump_table);

  Identifier *state_id = nullptr;

  if (scope_lit->stateful_) {
    state_id = new Identifier(TextSpan{}, "<scope-state>");
    ctx->set_type(state_id, state_ptr_type);
  }

  for (auto &block : blocks_) {
    auto &data              = block_data[block.get()];
    ir::BasicBlock::Current = data.index_;

    FnArgs<std::pair<Expression *, ir::Val>> before_args;
    FnArgs<Expression *> before_expr_args;

    if (scope_lit->stateful_) {
      before_args.pos_.emplace_back(state_id,
                                    ir::Val::Reg(alloc, state_ptr_type));
      before_expr_args.pos_.push_back(state_id);
    }
    auto[dispatch_table, result_type] =
        DispatchTable::Make(before_expr_args, data.before_os_, ctx);

    // TODO args?
    dispatch_table.EmitCall(before_args, result_type, ctx);

    block->EmitIR(ctx);
    auto yields = std::move(ctx->yields_stack_.back());

    FnArgs<Expression *> after_expr_args;
    FnArgs<std::pair<Expression *, ir::Val>> after_args;
    if (scope_lit->stateful_) {
      after_expr_args.pos_.push_back(state_id);
      after_args.pos_.emplace_back(state_id,
                                   ir::Val::Reg(alloc, state_ptr_type));
    }
    for (auto &yield : yields) { 
      after_expr_args.pos_.push_back(yield.expr_);
      after_args.pos_.emplace_back(yield.expr_, yield.val_);
    }

    std::tie(dispatch_table, result_type) =
        DispatchTable::Make(after_expr_args, data.after_os_, ctx);
    auto call_exit_result =
        dispatch_table.EmitCall(after_args, result_type, ctx)[0]
            .reg_or<ir::BlockSequence>();

    ir::BlockSeqJump(call_exit_result, jump_table);
  }

  {  // Landing block
    OverloadSet done_os;
    for (auto &decl : scope_lit->decls_) {
      if (decl.id_ == "done") {
        done_os.emplace_back(&decl, &ctx->type_of(&decl)->as<type::Function>());
      }
    }

    ir::BasicBlock::Current = land_block;

    FnArgs<Expression *> expr_args;
    FnArgs<std::pair<Expression *, ir::Val>> args;
    if (scope_lit->stateful_) {
      args.pos_.emplace_back(state_id, ir::Val::Reg(alloc, state_ptr_type));
      expr_args.pos_.push_back(state_id);
    }
    std::tie(dispatch_table, result_type) =
        DispatchTable::Make(expr_args, done_os, ctx);

    auto results = dispatch_table.EmitCall(args, result_type, ctx);
    if (scope_lit->stateful_) { state_type->EmitDestroy(alloc, ctx); }
    return results;
  }
}

base::vector<ir::RegisterOr<ir::Addr>> ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace ast
