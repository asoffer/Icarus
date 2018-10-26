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
  VERIFY_OR_RETURN(scope_type, name_);
  // TODO check the scope type makes sense.

  auto arg_types =
      args_.Transform([ctx, this](auto &arg) { return arg->VerifyType(ctx); });
  // TODO type check

  for (auto &block : blocks_) { block->VerifyType(ctx); }

  // TODO check that all the blocks make sense and emit errors

  // TODO compute what type this should return
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
  auto *scope_lit = backend::EvaluateAs<ScopeLiteral *>(name_.get(), ctx);

  auto init_block = IR::Func::Current->AddBlock();
  auto land_block = IR::Func::Current->AddBlock();

  IR::UncondJump(init_block);
  IR::BasicBlock::Current = init_block;

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
    IR::BlockIndex index_;
    BlockLiteral *lit_;
  };

  auto *jump_table =
      new std::unordered_map<AST::BlockLiteral const *, IR::BlockIndex>{
          {reinterpret_cast<AST::BlockLiteral const *>(0x1), init_block},
          {nullptr, land_block}};

  std::unordered_map<BlockNode *, BlockData> block_data;
  std::unordered_set<type::Type const *> state_types;
  for (auto &block : blocks_) {
    // TODO for now do lookup assuming it's an identifier.
    ASSERT(block->name_, Is<Identifier>());
    auto *decl = name_lookup.at(block->name_->as<Identifier>().token);
    auto &bseq = *backend::EvaluateAs<IR::BlockSequence>(decl, ctx).seq_;
    ASSERT(bseq.size() == 1u);

    OverloadSet os_before;
    for (auto &b : bseq[0]->before_) {
      auto *t = ctx->type_of(b.get());
      os_before.emplace_back(b.get(), t);
      state_types.insert(t->as<type::Function>().input[0]);
    }

    OverloadSet os_after;
    for (auto &a : bseq[0]->after_) {
      auto *t = ctx->type_of(a.get());
      os_after.emplace_back(a.get(), t);
      state_types.insert(t->as<type::Function>().input[0]);
    }
    auto block_index        = IR::Func::Current->AddBlock();
    block_data[block.get()] = {std::move(os_before), std::move(os_after),
                               block_index, bseq[0]};
    jump_table->emplace(bseq[0], block_index);
  }

  ASSERT(state_types.size() == 1u);
  auto *state_ptr_type = *state_types.begin();
  ASSERT(state_ptr_type, Is<type::Pointer>());
  IR::Register alloc = IR::Alloca(state_ptr_type->as<type::Pointer>().pointee);

  auto[dispatch_table, result_type] = DispatchTable::Make(
      args_.Transform(
          [](std::unique_ptr<Expression> const &expr) { return expr.get(); }),
      init_os, ctx);
  auto block_seq =
      EmitCallDispatch(
          args_.Transform([ctx](std::unique_ptr<Expression> const &expr) {
            return std::pair(const_cast<Expression *>(expr.get()),
                             expr->EmitIR(ctx)[0]);
          }),
          dispatch_table, result_type, ctx)[0]
          .reg_or<IR::BlockSequence>();
  IR::BlockSeqJump(block_seq, jump_table);

  for (auto &block : blocks_) {
    auto &data              = block_data[block.get()];
    IR::BasicBlock::Current = data.index_;

    auto *state_id = new Identifier(TextSpan{}, "<scope-state>");
    ctx->set_type(state_id, state_ptr_type);

    FnArgs<std::pair<Expression *, IR::Val>> args;
    args.pos_.emplace_back(state_id, IR::Val::Reg(alloc, state_ptr_type));

    FnArgs<Expression *> expr_args;
    expr_args.pos_.push_back(state_id);
    auto[dispatch_table, result_type] =
        DispatchTable::Make(expr_args, data.before_os_, ctx);

    // TODO args?
    EmitCallDispatch(args, dispatch_table, result_type, ctx);
    block->stmts_.EmitIR(ctx);

    // TODO always same set of args for before and after?
    std::tie(dispatch_table, result_type) =
        DispatchTable::Make(expr_args, data.after_os_, ctx);
    auto call_exit_result =
        EmitCallDispatch(args, dispatch_table, result_type, ctx)[0]
            .reg_or<IR::BlockSequence>();

    IR::BlockSeqJump(call_exit_result, jump_table);
  }

  IR::BasicBlock::Current = land_block;

  return {};
}

base::vector<IR::Register> ScopeNode::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
