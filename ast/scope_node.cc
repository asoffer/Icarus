#include "ast/scope_node.h"


#include <sstream>
#include "ast/access.h"
#include "ast/block_literal.h"
#include "ast/block_node.h"
#include "core/fn_args.h"
#include "ast/function_literal.h"
#include "ast/identifier.h"
#include "ast/scope_literal.h"
#include "backend/eval.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/func.h"
#include "misc/context.h"
#include "core/scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
using ::matcher::InheritsFrom;

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " ";
  if (!args_.empty()) { ss << "(" << args_.to_string() << ") "; }
  for (auto const &block : blocks_) { ss << block.to_string(n); }
  return ss.str();
}

void ScopeNode::assign_scope(core::Scope *scope) {
  scope_ = scope;
  name_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
  for (auto &block : blocks_) { block.assign_scope(scope); }
}

void ScopeNode::DependentDecls(base::Graph<Declaration *> *g,
                               Declaration *d) const {
  args_.Apply([g, d](auto const &expr) { expr->DependentDecls(g, d); });
  for (auto &block : blocks_) { block.DependentDecls(g, d); }
}

VerifyResult ScopeNode::VerifyType(Context *ctx) {
  ASSIGN_OR(return _, auto name_result, name_->VerifyType(ctx));

  auto arg_types =
      args_.Transform([ctx, this](auto &arg) { return arg->VerifyType(ctx); });
  // TODO type check

  for (auto &block : blocks_) { block.VerifyType(ctx); }

  // TODO check the scope type makes sense.
  if (!name_result.const_) {
    ctx->error_log()->NonConstantScopeName(name_->span);
    return VerifyResult::Error();
  }

  // TODO
  for (auto &block_node : blocks_) { block_node.stmts_.VerifyType(ctx); }

  // TODO check that all the blocks make sense and emit errors

  // TODO compute what type this should return
  // TODO can this evaluate to anything?
  // TODO constant is wrong.
  return VerifyResult::Constant(ctx->set_type(this, type::Void()));
}

void ScopeNode::ExtractJumps(JumpExprs *rets) const {
  for (auto &block : blocks_) { block.ExtractJumps(rets); }
}

ir::Results ScopeNode::EmitIr(Context *ctx) {
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

  absl::flat_hash_map<std::string, Declaration *> name_lookup;
  for (auto &decl : scope_lit->decls_) { name_lookup.emplace(decl.id_, &decl); }

  struct BlockData {
    OverloadSet before_os_, after_os_;
    ir::BlockIndex index_;
    BlockLiteral *lit_;
  };

  auto *jump_table =
      new absl::flat_hash_map<ast::BlockLiteral const *, ir::BlockIndex>{
          {reinterpret_cast<ast::BlockLiteral const *>(0x1), init_block},
          {nullptr, land_block}};

  absl::flat_hash_map<BlockNode *, BlockData> block_data;
  ir::Register alloc;
  type::Type const *state_ptr_type = nullptr, *state_type = nullptr;
  absl::flat_hash_set<type::Type const *> state_types;
  for (auto &block : blocks_) {
    // TODO for now do lookup assuming it's an identifier.
    ASSERT(block.name_, InheritsFrom<Identifier>());
    auto *decl = name_lookup.at(block.name_->as<Identifier>().token);
    // Guarnteed to be constant because all declarations inside a scope literal
    // are guaranteed to be constant.
    auto &bseq = *backend::EvaluateAs<ir::BlockSequence>(decl, ctx).seq_;
    ASSERT(bseq.size() == 1u);

    OverloadSet os_before;
    for (auto &b : bseq[0]->before_) {
      auto const& t = ctx->type_of(&b)->as<type::Function>();
      os_before.emplace_back(&b, &t);
      if (scope_lit->stateful_) { state_types.insert(t.input[0]); }
    }

    OverloadSet os_after;
    for (auto &a : bseq[0]->after_) {
      auto const &t = ctx->type_of(&a)->as<type::Function>();
      os_after.emplace_back(&a, &t);
      if (scope_lit->stateful_) { state_types.insert(t.input[0]); }
    }
    auto block_index   = ir::Func::Current->AddBlock();
    block_data[&block] = {std::move(os_before), std::move(os_after),
                          block_index, bseq[0]};
    jump_table->emplace(bseq[0], block_index);
  }

  Identifier *state_id = nullptr;
  core::FnArgs<type::Typed<Expression *>> typed_args;
  core::FnArgs<std::pair<Expression *, ir::Results>> ir_args;
  if (scope_lit->stateful_) {
    ASSERT(state_types.size() == 1u);
    state_ptr_type = *state_types.begin();
    ASSERT(state_ptr_type, InheritsFrom<type::Pointer>());
    state_type = state_ptr_type->as<type::Pointer>().pointee;
    alloc      = ir::TmpAlloca(state_type, ctx);
    state_type->EmitInit(alloc, ctx);
    state_id = new Identifier(TextSpan{}, "<scope-state>");

    typed_args.pos_emplace(state_id, ctx->set_type(state_id, state_ptr_type));
    ir_args.pos_emplace(state_id, ir::Results{alloc});
  }

  args_.ApplyWithIndex([&](auto &&index,
                           std::unique_ptr<Expression> const &expr) {
    if constexpr (std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
      typed_args.pos_emplace(expr.get(), ctx->type_of(expr.get()));
      ir_args.pos_emplace(expr.get(), expr.get()->EmitIr(ctx));
    } else {
      typed_args.named_emplace(
          std::piecewise_construct, std::forward_as_tuple(index),
          std::forward_as_tuple(expr.get(), ctx->type_of(expr.get())));
      ir_args.named_emplace(
          std::piecewise_construct, std::forward_as_tuple(index),
          std::forward_as_tuple(expr.get(), expr.get()->EmitIr(ctx)));
    }
  });

  auto [dispatch_table, result_type] =
      DispatchTable::Make(typed_args, init_os, ctx);
  auto block_seq = dispatch_table.EmitCall(ir_args, result_type, ctx)
                       .get<ir::BlockSequence>(0);
  ir::BlockSeqJump(block_seq, jump_table);

  for (auto &block : blocks_) {
    auto &data              = block_data[&block];
    ir::BasicBlock::Current = data.index_;

    core::FnArgs<std::pair<Expression *, ir::Results>> before_args;
    core::FnArgs<type::Typed<Expression *>> before_expr_args;

    if (scope_lit->stateful_) {
      before_args.pos_emplace(state_id, ir::Results{alloc});
      before_expr_args.pos_emplace(state_id, state_ptr_type);
    }
    auto [dispatch_table, result_type] =
        DispatchTable::Make(before_expr_args, data.before_os_, ctx);

    // TODO args?
    dispatch_table.EmitCall(before_args, result_type, ctx);

    block.EmitIr(ctx);
    auto yields = std::move(ctx->yields_stack_.back());

    core::FnArgs<type::Typed<Expression *>> after_expr_args;
    core::FnArgs<std::pair<Expression *, ir::Results>> after_args;
    if (scope_lit->stateful_) {
      after_expr_args.pos_emplace(state_id, state_ptr_type);
      after_args.pos_emplace(state_id, ir::Results{alloc});
    }
    for (auto &yield : yields) {
      after_expr_args.pos_emplace(yield.expr_, ctx->type_of(yield.expr_));
      after_args.pos_emplace(yield.expr_, yield.val_);
    }

    std::tie(dispatch_table, result_type) =
        DispatchTable::Make(after_expr_args, data.after_os_, ctx);
    auto call_exit_result =
        dispatch_table.EmitCall(after_args, result_type, ctx)
            .get<ir::BlockSequence>(0);

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

    core::FnArgs<type::Typed<Expression *>> expr_args;
    core::FnArgs<std::pair<Expression *, ir::Results>> args;
    if (scope_lit->stateful_) {
      args.pos_emplace(state_id, ir::Results{alloc});
      expr_args.pos_emplace(state_id, state_ptr_type);
    }
    std::tie(dispatch_table, result_type) =
        DispatchTable::Make(expr_args, done_os, ctx);

    auto results = dispatch_table.EmitCall(args, result_type, ctx);
    if (scope_lit->stateful_) { state_type->EmitDestroy(alloc, ctx); }
    return results;
  }
}

std::vector<ir::RegisterOr<ir::Addr>> ScopeNode::EmitLVal(Context *) {
  UNREACHABLE(this);
}
}  // namespace ast
